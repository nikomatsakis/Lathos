package com.smallcultfollowing.lathos;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

public class TestLinkedLog {
	
	public class TestLoggable implements Loggable {
		
		public final String name;
		public final String heading;
		public final Map<String, Object> fields = new HashMap<String, Object>();
		
		public TestLoggable(String name, String heading) {
			this.name = name;
			this.heading = heading;
		}

		@Override
		public Map<String, Object> logFields() {
			return fields;
		}

		@Override
		public String logHeading() {
			return heading;
		}

		@Override
		public LogId logId() {
			return new LogId(name, 1);
		}
		
	}
	
	DummyLogLink link;
	LinkedLog log;
	
	@Before public void init() {
		link = new DummyLogLink();
		log = new LinkedLog(link);		
	}
	
	@Test public void foo() {
		TestLoggable test1 = new TestLoggable("test1", "Heading1");
		test1.fields.put("x", "y");
		
		TestLoggable test2 = new TestLoggable("test2", "Heading2");
		test2.fields.put("test1", test1);
		
		link.returnValues.add(LList.llist(test1.logId(), test2.logId()));
		link.returnValues.add(null);
		
		log.log("Hello, %s! test1=%s, test2=%s.", "you", test1, test2);
		
		link.dumpMessages();
		link.assertNextMessage(
				"[{node, {id, \"Log0\", 1}, [{id, \"index\", 1}], [" +
				"{text, \"Hello, \"}, {text, \"you\"}, {text, \"! test1=\"}, " +
				"{link, \"test1.1\", {id, \"test1\", 1}}, {text, \", test2=\"}, " +
				"{link, \"test2.1\", {id, \"test2\", 1}}, {text, \".\"}]}]"
				);
		link.assertNextMessage(
				"[" +
				"{node, {id, \"test2\", 1}, [], [{text, \"Heading2\"}]}, " +
				"{node, {id, \"Log1\", 1}, [{id, \"test2\", 1}], [{text, \"test1: \"}, {link, \"test1.1\", {id, \"test1\", 1}}]}, " +
				"{node, {id, \"test1\", 1}, [], [{text, \"Heading1\"}]}, " +
				"{node, {id, \"Log2\", 1}, [{id, \"test1\", 1}], [{text, \"x: \"}, {text, \"y\"}]}" +
				"]"
				);
		link.assertAllMessages();
	}

}
