package com.smallcultfollowing.lathos;

public class Util {
	public static void appendEscapedString(StringBuilder sb, String str) {
		appendEscapedString(sb, str, 0, str.length());
	}
	public static void appendEscapedString(StringBuilder sb, String str, int min, int max) {
		sb.append('"');
		appendEscapedStringContents(sb, str, min, max);		
		sb.append('"');
	}
	public static void appendEscapedStringContents(StringBuilder sb, String str, int min, int max) 
	{
		for(int i = min; i < max; i++) {
			char c = str.charAt(i);
			if(c == '"') sb.append("\\\"");
			else if(c == '\\') sb.append("\\\\");
			else sb.append(c);
		}
	}
}
