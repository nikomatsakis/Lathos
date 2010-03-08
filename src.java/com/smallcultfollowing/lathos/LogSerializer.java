package com.smallcultfollowing.lathos;

import java.util.Map;

class LogSerializer {
	
	private final StringBuilder sb;
	private final Log log;
	
	public LogSerializer(Log log) {	
		this.log = log;
		this.sb = new StringBuilder("[");
	}
	
	public String result() {
		strikeTrailingComma();
		sb.append("]");
		return sb.toString();
	}

	private void appendText(
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

	private void strikeTrailingComma() {
		int length = sb.length();
		if(length > 2) {
			if(sb.charAt(length - 2) == ',' && sb.charAt(length - 1) == ' ')
				sb.setLength(length - 2);
		}
	}
	
	private void appendPrefixForErlangNode(LogId logId, LogId parentId)
	{
		sb.append("{node, ");
		logId.appendAsErlangTerm(sb);
		sb.append(", [");
		if(parentId != null)
			parentId.appendAsErlangTerm(sb);
		sb.append("], [");
	}
	
	private void appendSuffixForErlangNode() {
		strikeTrailingComma();
		sb.append("]}, "); // include a trailing comma because we always define erlang nodes in lists
	}
	
	public LList<LogId, Loggable> appendErlangNodeForLogMessage(
			LList<LogId, Loggable> links, 
			LogId id, 
			LogId parentId,
			String fmt, 
			Object[] args)
	{
		appendPrefixForErlangNode(id, parentId);
		
		int length = fmt.length();
		int upToIndex = 0;
		int upToArg = 0;
		int nextPercent;
		while((nextPercent = fmt.indexOf('%', upToIndex)) != -1) {
			appendText(fmt, upToIndex, nextPercent);
			if(nextPercent == length - 1) { // % is last char in string
				sb.append("{text, \"%\"}, ");
				upToIndex = nextPercent + 1;
			} else if(fmt.charAt(nextPercent + 1) == '%') {
				sb.append("{text, \"%\"}, ");
				upToIndex = nextPercent + 2;
			} else if(fmt.charAt(nextPercent + 1) == 's') {
				if(upToArg < args.length) {
					links = appendLink(links, args[upToArg]);
					upToArg += 1;
				} else {
					sb.append("{text, \"???\"}, ");
				}
				upToIndex = nextPercent + 2;
			}
		} 
		
		appendText(fmt, upToIndex, length);
		
		for(int i = upToArg; i < args.length; i++)
			links = appendLink(links, args[i]);
		
		appendSuffixForErlangNode();
		return links;
	}
	
	private LList<LogId, Loggable> appendLink(
			LList<LogId, Loggable> links, 
			Object object) 
	{
		if(object instanceof Loggable) {
			Loggable l = (Loggable) object;
			
			LogId objectId = l.logId();
			if(objectId == null)
				objectId = log.freshLogId();
			
			links = LList.add(objectId, l, links);
			sb.append("{link, ");
			Util.appendEscapedString(sb, objectId.toString());
			sb.append(", ");
			objectId.appendAsErlangTerm(sb);
			sb.append("}, ");
		} else {
			String str = object.toString();
			appendText(str, 0, str.length());
		}
		return links;
	}
	
	public LList<LogId, Loggable> appendErlangNodesForLoggable(
			LList<LogId, Loggable> links,
			LogId logId, 
			Loggable item)
	{
		appendPrefixForErlangNode(logId, null);
		String heading = item.logHeading();
		appendText(heading, 0, heading.length());
		appendSuffixForErlangNode();
		
		Map<String, Object> fields = item.logFields();
		for(Map.Entry<String, Object> entry : fields.entrySet()) {
			String key = entry.getKey();
			LogId fieldId = log.freshLogId();
			appendPrefixForErlangNode(fieldId, logId);
			
			sb.append("{text, \"");
			Util.appendEscapedStringContents(sb, key, 0, key.length());
			sb.append(": \"}, ");
			
			links = appendLink(links, entry.getValue());
			appendSuffixForErlangNode();
		}
		return links;
	}


	
}
