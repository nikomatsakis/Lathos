package com.smallcultfollowing.lathos.none;

import com.smallcultfollowing.lathos.model.Context;
import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Page;

public class NoneContext implements Context {
	
	public final NoneServer server;
	
	public NoneContext(NoneServer server) {
		super();
		this.server = server;
	}

	@Override
	public LathosServer server() {
		return server;
	}

	@Override
	public Context context() {
		return this;
	}

	@Override
	public String freshId() {
		return server.freshId();
	}

	@Override
	public Page pushTopLevel(String id, Object... title) {
		return server.getIndexPage();
	}

	@Override
	public Page pushChild(String id, Object... title) {
		return server.getIndexPage();
	}

	@Override
	public void push(Page page) {
	}

	@Override
	public void embedIn(Page page) {
	}

	@Override
	public void log(Object... line) {
	}

	@Override
	public void append(Object... line) {
	}

	@Override
	public void pop(Page page) {
	}

	@Override
	public CustomOutput link(Page page, Object... content) {
		return NoneCustomOutput.instance;
	}

	@Override
	public Page topPage() {
		return server.getIndexPage();
	}

}
