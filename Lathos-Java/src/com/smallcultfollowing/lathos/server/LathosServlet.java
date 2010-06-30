package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.io.Writer;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicInteger;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringEscapeUtils;

import com.smallcultfollowing.lathos.context.AbstractContext;
import com.smallcultfollowing.lathos.context.Context;
import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.DataRenderer;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.PageContent;

public class LathosServlet
extends HttpServlet
implements LathosServer 
{
	private static final long serialVersionUID = -7831061598026617576L;
	
	private final AtomicInteger id = new AtomicInteger();
	private final Map<String, List<Page>> topLevelPages = new HashMap<String, List<Page>>();
	private DataRenderer[] customDataRenderers = new DataRenderer[0];
	
	public synchronized void addDataRenderer(DataRenderer dr) {
		int length = customDataRenderers.length + 1;
		DataRenderer[] newDataRenderers = new DataRenderer[length];
		System.arraycopy(customDataRenderers, 0, newDataRenderers, 0, length - 1);
		newDataRenderers[length] = dr;
		customDataRenderers = newDataRenderers;
	}
	
	private synchronized DataRenderer[] dataRenderers() {
		return customDataRenderers;
	}
	
	@Override
	public synchronized void addTopLevelPage(Page page) {
		List<Page> list = topLevelPages.get(page.getId());
		if(list == null) {
			list = new LinkedList<Page>();
			topLevelPages.put(page.getId(), list);
		}
		if(!list.contains(page))
			list.add(page);
	}
	
	@Override
	public String freshId() {
		int i = id.getAndIncrement();
		return "page" + Integer.toHexString(i);
	}
	
	private void renderError(Writer out, String url, String desc, Object... args) throws IOException {
		out.write("<html><head><title>Error</title></head>");
		out.write(String.format("<body><h1>Error with %s</h1>", StringEscapeUtils.escapeHtml(url)));
		out.write(String.format("%s", StringEscapeUtils.escapeHtml(String.format(desc, args))));
		out.write("</body></html>");
	}
	
	public String url(Page page) {
		StringBuilder sb = new StringBuilder();
		addURL(page, sb);
		return sb.toString();
	}
	
	private void addURL(Page page, StringBuilder sb) {
		if(page.getParent() != null) {
			addURL(page.getParent(), sb);
			sb.append("/");
		}
		
		try {
			sb.append(URLEncoder.encode(page.getId(), "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	public synchronized void renderURL(String url, Writer out)
	throws IOException
	{
		String[] ids = url.split("/");
		
		if(ids.length == 0) {
			renderError(out, url, "Empty URL");
			return;
		}
		
		for(int i = 0; i < ids.length; i++) {
			try {
				ids[i] = URLDecoder.decode(ids[i], "UTF-8");
			} catch (UnsupportedEncodingException e) {
				renderError(out, url, "Id #%d (%s) contains invalid characters: %s", i, ids[i], e.toString());
				return;
			}
		}
		
		// Note: Since all URLs begin with "/", ignore first component!
		List<Page> pages = topLevelPages.get(ids[1]);
		int nextIndex = 2;
		while(true) {
			if(pages == null || pages.size() == 0) {
				renderError(out, url, "Id #%d (%s) yields no pages.", nextIndex - 1, ids[nextIndex - 1]);
				return;
			}
			
			if(nextIndex == ids.length)
				break;
			
			List<Page> newPages = new LinkedList<Page>();
			for(Page page : pages) {
				for(PageContent content : page.contents()) {
					if(content instanceof Page) {
						Page subPage = (Page) content;
						if(subPage.getId().equals(ids[nextIndex]))
							newPages.add(subPage);
					}
				}
			}
			
			pages = newPages;
			nextIndex++;
		} 
		
		HtmlOutput output = new HtmlOutput(this, pages, out);
		
		for(Page page : pages) {
			page.renderInPage(output);
		}
	}

	@Override
	public Context context() {
		return new AbstractContext(this);
	}

	@Override
	public void addToLine(Line previousLine, Object o) {
		if(o instanceof String)
			previousLine.addText((String)o);
		else if (o instanceof Number)
			previousLine.addNumber((Number)o);
		else if (o instanceof CustomOutput)
			previousLine.addContent((CustomOutput)o);
		else {
			for(DataRenderer dr : dataRenderers()) {
				if(dr.addToLine(previousLine, o))
					return;
			}
			previousLine.addText(o.toString());
		}
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException 
	{
		PrintWriter writer = resp.getWriter();
		renderURL(req.getRequestURI(), writer);
		writer.close();
	}
	
}
