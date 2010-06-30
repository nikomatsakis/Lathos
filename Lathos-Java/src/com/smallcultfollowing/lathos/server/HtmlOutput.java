package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.io.Writer;
import java.util.Collections;
import java.util.List;

import org.apache.commons.lang.StringEscapeUtils;

import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;

public class HtmlOutput implements Output {
	public final LathosServlet server;
	public final List<Page> topPages;
	public final Writer writer;

	public HtmlOutput(
			LathosServlet server,
			List<Page> topPages, 
			Writer writer
	) {
		super();
		this.server = server;
		this.topPages = Collections.unmodifiableList(topPages);
		this.writer = writer;
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
		writer.write("<a href='");
		writer.write(server.url(target));
		writer.write("'>");
	}

	@Override
	public void endLink(Page target) throws IOException {
		writer.write("</a>");
	}

}
