package com.smallcultfollowing.lathos;

import java.util.ArrayList;
import java.util.List;

import junit.framework.Assert;

public class DummyLogLink extends LogLink {
	
	public final List<LList<LogId, Void>> returnValues = new ArrayList<LList<LogId, Void>>();
	public final List<String> messages = new ArrayList<String>();
	public int position = 0;

	@Override
	public LList<LogId, Void> createNode(String msg) {
		int index = messages.size();
		messages.add(msg);
		return returnValues.get(index);
	}
	
	public void assertNextMessage(String msg) {
		Assert.assertEquals(msg, messages.get(position++));
	}

	public void assertAllMessages() {
		Assert.assertEquals(position, messages.size());
	}

	public void dumpMessages() {
		for(String message : messages)
			System.err.printf("Message: %s\n", message);
	}

}
