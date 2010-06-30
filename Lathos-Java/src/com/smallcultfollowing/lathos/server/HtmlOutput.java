package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringEscapeUtils;

import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;

public class HtmlOutput implements Output {
	public final LathosServlet server;
	public final List<Page> topPages;
	public final PrintWriter writer;
	private int maxId;

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
	
	public String freshId() {
		return "id"+maxId++;
	}

	@Override
	public List<Page> topPages() {
		return topPages;
	}

	@Override
	public void outputObject(Object content) {
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
	public String startDiv() throws IOException {
		String id = freshId();
		writer.printf("<DIV id='%s' class='log initiallyOpen'>", id);
		return id;
	}

	@Override
	public void endDiv() throws IOException {
		writer.println("</DIV>");
	}

	@Override
	public void startLine() throws IOException {
		writer.println("<p>");
	}

	@Override
	public void endLine() throws IOException {
		writer.println("</p>");
	}

}
