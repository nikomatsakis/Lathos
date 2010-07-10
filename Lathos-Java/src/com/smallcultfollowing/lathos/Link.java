package com.smallcultfollowing.lathos;

import java.io.IOException;


public class Link extends LineImpl {
	
	public final Page page;
	
	public Link(Page page) {
		super();
		this.page = page;
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		output.startLink(page);
		super.renderInLine(output);
		output.endLink(page);
	}

}
