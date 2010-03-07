package com.smallcultfollowing.lathos;

import java.util.Map;

public class Log {
	private int counter;
	private LList<LogId> idStack = new LList<LogId>(LogId.index, null);
	private LogLink link;
	
	public LogId freshLogId() {
		int value = counter++;
		return new LogId("Log"+value, 1);
	}
	
	private void appendPrefixForErlangNode(StringBuilder sb, LogId logId, LogId parentId)
	{
		sb.append("{node, ");
		logId.appendAsErlangTerm(sb);
		sb.append(", [");
		if(parentId != null)
			parentId.appendAsErlangTerm(sb);
		sb.append("], [");
	}
	
	private void appendSuffixForErlangNode(StringBuilder sb) {
		sb.append("]}, "); // include a trailing comma because we always define erlang nodes in lists
	}
	
	private LList<Loggable> appendErlangNodeForLogMessage(
			LList<Loggable> links, 
			StringBuilder sb, 
			LogId id, 
			String fmt, 
			Object[] args)
	{
		appendPrefixForErlangNode(sb, id, idStack.elem);
		
		int length = fmt.length();
		int upToIndex = 0;
		int upToArg = 0;
		int nextPercent;
		while((nextPercent = fmt.indexOf('%', upToIndex)) != -1) {
			appendText(sb, fmt, upToIndex, nextPercent);
			if(nextPercent == length - 1) { // % is last char in string
				sb.append("{text, \"%\"}, ");
				upToIndex = nextPercent + 1;
			} else if(fmt.charAt(nextPercent + 1) == '%') {
				sb.append("{text, \"%\"}, ");
				upToIndex = nextPercent + 2;
			} else if(fmt.charAt(nextPercent + 1) == 's') {
				links = appendLink(links, sb, args[upToArg]);
				upToArg += 1;
				upToIndex = nextPercent + 2;
			}
		} 
		
		appendText(sb, fmt, upToIndex, length);
		
		for(int i = upToArg; i < args.length; i++)
			links = appendLink(links, sb, args[i]);
		
		appendSuffixForErlangNode(sb);
		return links;
	}
	
	private LList<Loggable> appendLink(
			LList<Loggable> links, 
			StringBuilder sb, 
			Object object) 
	{
		if(object instanceof Loggable) {
			Loggable l = (Loggable) object;
			links = links.prefix(l);
			LogId objectId = l.logId();
			sb.append("{link, ");
			Util.appendEscapedString(sb, objectId.toString());
			sb.append(", ");
			objectId.appendAsErlangTerm(sb);
			sb.append("}, ");
		} else {
			String str = object.toString();
			appendText(sb, str, 0, str.length());
		}
		return links;
	}
	
	private LList<Loggable> appendErlangNodesForLoggable(
			LList<Loggable> links,
			StringBuilder sb,
			LogId logId, 
			Loggable item)
	{
		appendPrefixForErlangNode(sb, logId, null);
		String heading = item.logHeading();
		appendText(sb, heading, 0, heading.length());
		appendSuffixForErlangNode(sb);
		
		Map<String, Object> fields = item.logFields();
		for(Map.Entry<String, Object> entry : fields.entrySet()) {
			String key = entry.getKey();
			LogId fieldId = freshLogId();
			appendPrefixForErlangNode(sb, fieldId, logId);
			
			sb.append("{text, \"");
			Util.appendEscapedStringContents(sb, key, 0, key.length());
			sb.append(": \"}, ");
			
			links = appendLink(links, sb, entry.getValue());
			appendSuffixForErlangNode(sb);
		}
		return links;
	}

	private void achieveClosure(LList<LogId> missing, LList<Loggable> links) {
		while(missing != null) {
			StringBuilder sb = new StringBuilder("[");
			LList<Loggable> nextLinks = null;
			
			while(links != null) {
				LogId logId = links.elem.logId();
				if(missing.contains(logId))
					nextLinks = appendErlangNodesForLoggable(nextLinks, sb, logId, links.elem);
				missing = missing.next;
			}
			
			sb.append("]");
			
			links = nextLinks;
			missing = link.createNode(sb.toString());
		}
	}

	public LogId log(String fmt, Object... args) {
		LogId id = freshLogId();
		StringBuilder sb = new StringBuilder("[");
		LList<Loggable> links = appendErlangNodeForLogMessage(null, sb, id, fmt, args);
		sb.append("]");
		LList<LogId> missing = link.createNode(sb.toString());
		achieveClosure(missing, links);
		return id;
	}

	private void appendText(
			StringBuilder sb, 
			String text, 
			int fromIndex,
			int toIndex) 
	{
		if(fromIndex < toIndex - 1) {
			sb.append("{text, ");
			Util.appendEscapedString(sb, text, fromIndex, toIndex);
			sb.append("}, ");
		}
	}

	public LogId indent(String fmt, Object... args) {
		LogId id = log(fmt, args);
		idStack = new LList<LogId>(id, idStack);
		return idStack.elem;
	}
	
	public void undent(LogId top) {
		assert top == idStack.elem : "Unmatched indent and undent";
		idStack = idStack.next;
	}

}
