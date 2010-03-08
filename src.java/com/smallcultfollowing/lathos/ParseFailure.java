package com.smallcultfollowing.lathos;

import java.io.StreamTokenizer;

@SuppressWarnings("serial")
public class ParseFailure extends Exception {
	
	final int line;
	final String encountered;
	final String expected;

	public ParseFailure(StreamTokenizer tok, String expected) {
		line = tok.lineno();
		switch(tok.ttype) {
		case StreamTokenizer.TT_EOF: encountered = "<EOF>"; break;
		case StreamTokenizer.TT_EOL: encountered = "<EOL>"; break;
		case StreamTokenizer.TT_NUMBER: encountered = "<number:"+tok.nval+">"; break;
		case StreamTokenizer.TT_WORD: encountered = "<word:"+tok.sval+">"; break;
		case '"': encountered = "<string:"+tok.sval+">"; break;
		default: encountered = "'" + ((char)tok.ttype) + "'"; break;
		}
		this.expected = expected;
	}
	
	public String toString() {
		return String.format("On line %d, expected %s but found %s.", line, expected, encountered);
	}

}
