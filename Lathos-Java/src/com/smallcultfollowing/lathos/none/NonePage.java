package com.smallcultfollowing.lathos.none;

import java.io.IOException;

import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.PageContent;

public class NonePage implements Page {

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
