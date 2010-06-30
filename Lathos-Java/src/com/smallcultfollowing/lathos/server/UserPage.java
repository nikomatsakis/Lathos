package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.util.LinkedList;

import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.PageContent;

public class UserPage implements Page {
	
	public final String id;
	public final Page parent; // nullable
	private final LinkedList<PageContent> contents = new LinkedList<PageContent>();
	
	public UserPage(String id, Page parent) {
		super();
		this.id = id;
		this.parent = parent;
		
		if(parent != null)
			parent.addContent(this);
	}

	@Override
	public synchronized void renderInPage(Output out) throws IOException {
		out.startDiv();
		
		if(out.topPages().contains(this)) {
			out.startLine();
			printBreadcrumbs(this, out);
			out.endLine();
		}
		
		for(PageContent content : contents) {
			content.renderInPage(out);
		}
		
		out.endDiv();
	}

	private static void printBreadcrumbs(Page page, Output out) throws IOException {
		if(page.getParent() != null) {
			printBreadcrumbs(page.getParent(), out);
			out.outputText(" → ");
		}
		
		out.startLink(page);
		out.outputText(page.getId());
		out.endLink(page);
	}

	@Override
	public String getId() {
		return id;
	}
	
	@Override
	public Page getParent() {
		return parent;
	}

	@Override
	public synchronized void addContent(PageContent content) {
		contents.add(content);
	}

	@Override
	public synchronized PageContent[] contents() {
		return contents.toArray(new PageContent[contents.size()]);
	}

	@Override
	public void renderInLine(Output output) throws IOException {
		output.startLink(this);
		output.outputText(getId());
		output.endLink(this);
	}

}
