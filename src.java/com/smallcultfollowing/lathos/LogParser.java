package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.Reader;
import java.io.StreamTokenizer;

public class LogParser {
	
	public static final LogParser parser = new LogParser(); 

	LList<LogId, Void> parseStreamAsLogIdList(Reader reader) throws IOException, ParseFailure {
		StreamTokenizer tok = new StreamTokenizer(reader);
		tok.slashSlashComments(false);
		tok.slashStarComments(false);
		tok.quoteChar('"');
		tok.parseNumbers();
		tok.eolIsSignificant(false);
		tok.lowerCaseMode(false);
		
		tok.nextToken();
		LList<LogId, Void> result = parseList(tok);
		if(tok.ttype != StreamTokenizer.TT_EOF)
			throw new ParseFailure(tok, "<EOF>");
		return result;
	}

	LList<LogId, Void> parseList(StreamTokenizer tok) throws IOException, ParseFailure {
		expect(tok, '[');
		LList<LogId, Void> result = parseListContents(tok);
		expect(tok, ']');
		return result;
	}

	private void expect(StreamTokenizer tok, char c) throws ParseFailure, IOException {
		if(tok.ttype != c)
			throw new ParseFailure(tok, Character.toString(c));
		tok.nextToken();
	}

	private LList<LogId, Void> parseListContents(StreamTokenizer tok) throws IOException, ParseFailure {
		if(tok.ttype == '{') {
			LogId id = parseId(tok);
			LList<LogId, Void> next;
			if(tok.ttype == ',') {
				tok.nextToken();
				next = parseListContents(tok);
			} else {
				next = null;
			}			
			return new LList<LogId, Void>(id, null, next);
		} else return null;
	}

	private LogId parseId(StreamTokenizer tok) throws ParseFailure, IOException {
		// { id, "name", version }
		expect(tok, '{');
		if(tok.ttype != StreamTokenizer.TT_WORD || !tok.sval.equals("id"))
			throw new ParseFailure(tok, "\"id\"");
		tok.nextToken();
		expect(tok, ',');
		
		if(tok.ttype != '"')
			throw new ParseFailure(tok, "quoted string");
		String name = tok.sval;
		tok.nextToken();
		expect(tok, ',');
		
		if(tok.ttype != StreamTokenizer.TT_NUMBER)
			throw new ParseFailure(tok, "version");
		int version = (int) tok.nval;
		tok.nextToken();
		expect(tok, '}');
		
		return new LogId(name, version);
	}


}
