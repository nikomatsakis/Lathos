package com.smallcultfollowing.lathos;

import java.io.IOException;


public class NonePage implements Page {
	
	public static final NonePage Page = new NonePage();

	@Override
	public void renderInPage(Output out) throws IOException {
	}

	@Override
	public void renderInLine(Output output) throws IOException {
	}

	@Override
	public String getId() {
		return "none";
	}

	@Override
	public Page getParent() {
		return null;
	}

	@Override
	public void addContent(PageContent content) {
	}

}
