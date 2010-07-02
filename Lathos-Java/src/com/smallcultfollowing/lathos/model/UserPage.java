package com.smallcultfollowing.lathos.model;

import java.io.IOException;
import java.util.LinkedList;


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
		out.startPage(this);
		
		if(out.topPages().contains(this)) {
			out.startPar();
			printBreadcrumbs(this, out);
			out.endPar();
		}
		
		for(PageContent content : contents) {
			content.renderInPage(out);
		}
		
		out.endPage(this);
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
	public void renderInLine(Output output) throws IOException {
		output.startLink(this);
		output.outputText(getId());
		output.endLink(this);
	}

}
