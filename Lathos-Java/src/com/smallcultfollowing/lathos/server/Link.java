package com.smallcultfollowing.lathos.server;

import java.io.IOException;

import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;

public class Link implements CustomOutput {
	
	private final Page page;
	private final Object content;
	
	public Link(Page page, Object content) {
		super();
		assert page != null;
		this.page = page;
		this.content = content;
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		output.startLink(page);
		if(content != null)
			output.outputObject(content);
		else
			output.outputText(page.getId());
		output.endLink(page);
	}

}
