package com.smallcultfollowing.lathos;

import static com.smallcultfollowing.lathos.LList.llist;

import java.io.IOException;
import java.io.StringReader;

import org.junit.Assert;
import org.junit.Test;

public class TestLogParser {
	
	public final LogId test1 = new LogId("test", 1);
	public final LogId test22 = new LogId("test", 22);
	public final LogId foo0 = new LogId("foo", 0);
	public final LogId bar0 = new LogId("bar", 0);
	
	public LList<LogId, Void> parseString(String str) throws IOException, ParseFailure {
		return LogParser.parser.parseStreamAsLogIdList(new StringReader(str));
	}

	@Test
	public void emptyList() throws IOException, ParseFailure {
		Assert.assertNull(parseString("[]"));
	}
	
	@Test
	public void oneId() throws IOException, ParseFailure {
		Assert.assertEquals(llist(test1), parseString("[{id,\"test\",1}]"));
	}
	
	@Test
	public void twoIds() throws IOException, ParseFailure {
		Assert.assertEquals(llist(test1, foo0), parseString("[{id,\"test\",1}, { id , \"foo\", 0 }]"));
	}
	
	@Test
	public void noSpaces() throws IOException, ParseFailure {
		Assert.assertEquals(llist(test22, foo0), parseString("[{id,\"test\",22},{id,\"foo\",0}]"));
	}
	
}
