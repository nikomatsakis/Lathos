package com.smallcultfollowing.lathos.none;

import com.smallcultfollowing.lathos.model.Context;
import com.smallcultfollowing.lathos.model.DataRenderer;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.Page;

public class NoneServer implements LathosServer {
	
	Page indexPage = new NonePage();

	@Override
	public Page getIndexPage() {
		return indexPage;
	}

	@Override
	public void registerPage(Page page) {
	}

	@Override
	public String freshId() {
		return "none";
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

}
