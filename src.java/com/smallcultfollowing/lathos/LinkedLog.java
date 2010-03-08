package com.smallcultfollowing.lathos;

import java.util.Map;

public class LinkedLog implements Log {
	private int counter;
	private LList<LogId, Void> idStack = new LList<LogId, Void>(LogId.index, null, null);
	private final LogLink link;
	
	public LinkedLog(LogLink link) {
		this.link = link;
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#freshLogId()
	 */
	public LogId freshLogId() {
		int value = counter++;
		return new LogId("Log"+value, 1);
	}
	
	private void strikeTrailingComma(StringBuilder sb) {
		int length = sb.length();
		if(length > 2) {
			if(sb.charAt(length - 2) == ',' && sb.charAt(length - 1) == ' ')
				sb.setLength(length - 2);
		}
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
		strikeTrailingComma(sb);
		sb.append("]}, "); // include a trailing comma because we always define erlang nodes in lists
	}
	
	private LList<LogId, Loggable> appendErlangNodeForLogMessage(
			LList<LogId, Loggable> links, 
			StringBuilder sb, 
			LogId id, 
			LogId parentId,
			String fmt, 
			Object[] args)
	{
		appendPrefixForErlangNode(sb, id, parentId);
		
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
	
	private LList<LogId, Loggable> appendLink(
			LList<LogId, Loggable> links, 
			StringBuilder sb, 
			Object object) 
	{
		if(object instanceof Loggable) {
			Loggable l = (Loggable) object;
			LogId objectId = l.logId();
			links = LList.add(objectId, l, links);
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
	
	private LList<LogId, Loggable> appendErlangNodesForLoggable(
			LList<LogId, Loggable> links,
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

	private void achieveClosure(LList<LogId, Void> missing, LList<LogId, Loggable> links) {
		while(missing != null) {
			StringBuilder sb = new StringBuilder("[");
			LList<LogId, Loggable> nextLinks = null;
			
			while(links != null) {
				LogId logId = links.elemE;
				if(LList.containsE(missing, logId))
					nextLinks = appendErlangNodesForLoggable(nextLinks, sb, logId, links.elemF);
				links = links.next;
			}
			
			strikeTrailingComma(sb);
			sb.append("]");
			
			links = nextLinks;
			missing = link.createNode(sb.toString());
		}
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#log(java.lang.String, java.lang.Object)
	 */
	public LogId log(String fmt, Object... args) {
		LogId id = freshLogId();
		StringBuilder sb = new StringBuilder("[");
		LList<LogId, Loggable> links = appendErlangNodeForLogMessage(null, sb, id, idStack.elemE, fmt, args);
		strikeTrailingComma(sb);
		sb.append("]");
		LList<LogId, Void> missing = link.createNode(sb.toString());
		achieveClosure(missing, links);
		return id;
	}

	private void appendText(
			StringBuilder sb, 
			String text, 
			int fromIndex,
			int toIndex) 
	{
		if(fromIndex < toIndex) {
			sb.append("{text, ");
			Util.appendEscapedString(sb, text, fromIndex, toIndex);
			sb.append("}, ");
		}
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#indent(java.lang.String, java.lang.Object)
	 */
	public LogId indent(String fmt, Object... args) {
		LogId id = log(fmt, args);
		idStack = LList.add(id, null, idStack);
		return idStack.elemE;
	}
	
	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#undent(com.smallcultfollowing.lathos.LogId)
	 */
	public void undent(LogId top) {
		assert top == idStack.elemE : "Unmatched indent and undent";
		idStack = idStack.next;
	}

}
