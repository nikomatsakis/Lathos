package com.smallcultfollowing.lathos;


public class NoneServer implements LathosServer {
	public static final NoneServer Server = new NoneServer();
	
	Page indexPage = NonePage.Page;

	@Override
	public Page getIndexPage() {
		return indexPage;
	}

	@Override
	public void registerPage(Page page) {
	}

	@Override
	public Context context() {
		return new NoneContext(this);
	}

	@Override
	public void addDataRenderer(DataRenderer dr) {
	}

	@Override
	public void addToLine(Line line, Object o) {
	}

	@Override
	public void join() {
	}

	@Override
	public void stop() {
	}

}
