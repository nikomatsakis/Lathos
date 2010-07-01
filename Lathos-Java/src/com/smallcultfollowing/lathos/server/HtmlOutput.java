package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;
import java.util.Stack;

import org.apache.commons.lang.StringEscapeUtils;

import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;

public class HtmlOutput implements Output {
	public final LathosServlet server;
	public final List<Page> topPages;
	public final PrintWriter writer;
	private int maxId;
	private int currentColor;
	private LinkedList<String> idStack = new LinkedList<String>();
	
	private final String[] backgroundColors = new String[] {	    "E6FAFF",
	    "B2B091",
	    "FFFDE9",
	    "CCA6B2",
	    "B29AA2"
	};

	public HtmlOutput(
			LathosServlet server,
			List<Page> topPages, 
			PrintWriter writer
	) {
		super();
		this.server = server;
		this.topPages = Collections.unmodifiableList(topPages);
		this.writer = writer;
	}
	
	private String freshId() {
		return "id"+maxId++;
	}
	
	private String nextBackgroundColor() {
		return backgroundColors[(currentColor++) % backgroundColors.length];
	}

	@Override
	public List<Page> topPages() {
		return topPages;
	}

	@Override
	public void outputText(String content) throws IOException {
		StringEscapeUtils.escapeHtml(writer, content);
	}

	@Override
	public void startLink(Page target) throws IOException {
		writer.print("<a href='");
		writer.print(server.url(target));
		writer.print("'>");
	}

	@Override
	public void endLink(Page target) throws IOException {
		writer.print("</a>");
	}

	@Override
	public String startPage(Page page) throws IOException {
		String parentId = (idStack.isEmpty() ? "" : idStack.peek());
		String id = freshId();
		String color = nextBackgroundColor();
		writer.printf(
				"<DIV id='%s' class='log initiallyOpen' style='background-color: #%s'>", 
				id, color);
		idStack.add(id);
		
		writer.printf(
				"<A href='#%s'>&#8689;</A>&nbsp;"+                                   // up-left arrow
				//"<SPAN onclick='toggleId(\"%s\")'>&#9660;</SPAN>&nbsp;"+ // down arrow 
				"<A href='#' onclick='toggleId(\"%s\")'>&#9660;</A>&nbsp;"+ // down arrow 
				"<A href='%s'>&#9650;</A>",                                          // up arrow
				parentId,
				id,
				server.url(page)
				);
		
		return id;
	}

	@Override
	public void endPage(Page page) throws IOException {
		idStack.removeLast();
		writer.println("</DIV>");
	}

	@Override
	public void startPar() throws IOException {
		writer.println("<p>");
	}

	@Override
	public void endPar() throws IOException {
		writer.println("</p>");
	}

	@Override
	public void startBold() throws IOException {
		writer.print("<b>");
	}

	@Override
	public void endBold() throws IOException {
		writer.print("</b>");
	}

}
