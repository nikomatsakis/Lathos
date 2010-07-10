package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.LinkedList;
import java.util.List;

import org.apache.commons.lang.StringEscapeUtils;


public class HtmlOutput implements Output {
	public final HttpLathosServer server;
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
			HttpLathosServer server,
			List<Page> topPages, 
			PrintWriter writer
	) {
		super();
		this.server = server;
		this.topPages = Collections.unmodifiableList(topPages);
		this.writer = writer;
	}
	
	private String freshId() {
		return "id"+(maxId++);
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
		// Open a <DIV> and generate a unique id for it:
		String parentId = (idStack.isEmpty() ? "" : idStack.getLast());
		String id = freshId();
		String color = nextBackgroundColor();
		writer.printf(
				"<DIV id='%s' class='log initiallyOpen' style='background-color: #%s'>", 
				id, color);
		idStack.add(id);
		
		// Up-left arrow to go to parent:
		writer.printf(
				"<A href='#%s'>&#8689;</A>",
				parentId
		);
		
		// Down arrow to hide:
		writer.printf(
				"&nbsp;<SPAN onclick='toggleId(\"%s\")'>&#9660;</SPAN>",
				id
		);
		
		// Up arrow to magnify:
		if(page != null) {
			writer.printf(
					"&nbsp;<A href='%s'>&#9650;</A>",
					server.url(page)
			);
		}
		
		return id;
	}

	@Override
	public void endPage(Page page) throws IOException {
		idStack.removeLast();
		writer.println("</DIV>");
	}

	@Override
	public void startPar() throws IOException {
		writer.println("<P class='content'>");
	}

	@Override
	public void endPar() throws IOException {
		writer.println("</P>");
	}

	@Override
	public void startBold() throws IOException {
		writer.print("<B class='content'>");
	}

	@Override
	public void endBold() throws IOException {
		writer.print("</B>");
	}

	@Override
	public void startTable() throws IOException {
		writer.print("<TABLE class='content'>");
	}

	@Override
	public void startRow() throws IOException {
		writer.print("<TR>");
	}

	@Override
	public void startColumn() throws IOException {
		writer.print("<TD>");
	}

	@Override
	public void endColumn() throws IOException {
		writer.print("</TD>");
	}

	@Override
	public void endRow() throws IOException {
		writer.print("</TR>");
	}

	@Override
	public void endTable() throws IOException {
		writer.print("</TABLE>");
	}

	@Override
	public void outputObject(Object content) throws IOException {
		LineImpl line = new LineImpl();
		server.addToLine(line, content);
		line.renderInLine(this);
	}

}
