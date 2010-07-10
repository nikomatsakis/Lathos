package com.smallcultfollowing.lathos;


public class NoneContext implements Context {
	public static final NoneContext Context = new NoneContext(NoneServer.Server);

	public final LathosServer server;
	
	public NoneContext(LathosServer server) {
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
	public Page pushTopLevel(String id, Object... title) {
		return NonePage.Page;
	}

	@Override
	public Page pushLinkedChild(String id, Object... title) {
		return NonePage.Page;
	}

	@Override
	public Page pushEmbeddedChild(String id, Object... title) {
		return NonePage.Page;
	}

	@Override
	public Page push(Page page) {
		return page;
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
		return NonePage.Page;
	}

	@Override
	public void pushDisabledPage() {
	}

}
