package com.smallcultfollowing.lathos.server;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.atomic.AtomicReference;

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
import com.smallcultfollowing.lathos.model.UserPage;

public class LathosServlet
extends HttpServlet
implements LathosServer 
{
	private static final long serialVersionUID = -7831061598026617576L;
	
	// Accessible with or without lock:
	private final AtomicInteger id = new AtomicInteger();
	
	// Only MODIFIED under lock but may be READ without lock:
	private final ConcurrentHashMap<Page, Boolean> registeredPages = new ConcurrentHashMap<Page, Boolean>();
	
	// Only accessed under lock:
	private final Map<String, List<Page>> topLevelPages = new HashMap<String, List<Page>>();
	
	// Only accessed under lock:
	private final Map<Page, List<Page>> subpages = new HashMap<Page, List<Page>>();
	
	// Only MODIFIED under lock but may be READ without lock:
	//    Contents of the array are NEVER modified once stored in this field.
	private final AtomicReference<DataRenderer[]> customDataRenderers = new AtomicReference<DataRenderer[]>(new DataRenderer[0]);
	
	// We always add a special "indexPage" to start.
	private final Page indexPage = new UserPage("index", null);
	
	public LathosServlet() {
		registerPage(indexPage);
	} 
	
	public synchronized void addDataRenderer(DataRenderer dr) {
		DataRenderer[] oldDataRenderers = customDataRenderers.get();
		int length = oldDataRenderers.length + 1;
		DataRenderer[] newDataRenderers = new DataRenderer[length];
		System.arraycopy(oldDataRenderers, 0, newDataRenderers, 0, length - 1);
		newDataRenderers[length] = dr;
		customDataRenderers.set(newDataRenderers);
	}
	
	public DataRenderer[] dataRenderers() {
		return customDataRenderers.get();
	}
	
	@Override
	public String freshId() {
		int i = id.getAndIncrement();
		return "page" + Integer.toHexString(i);
	}
	
	private void renderError(PrintWriter out, String url, String desc, Object... args) throws IOException {
		out.write("<html><head><title>Error</title></head>");
		out.write(String.format("<body><h1>Error with %s</h1>", StringEscapeUtils.escapeHtml(url)));
		out.write(String.format("%s", StringEscapeUtils.escapeHtml(String.format(desc, args))));
		out.write("</body></html>");
	}
	
	public String url(Page page) {
		registerPage(page);
		
		StringBuilder sb = new StringBuilder();
		appendURL(page, sb);
		return sb.toString();
	}
	
	private synchronized <K> void addToList(Map<K, List<Page>> map, K key, Page page) {
		if(registeredPages.get(page) == null) {
			List<Page> list = map.get(key);
			if(list == null) {
				list = new LinkedList<Page>();
				map.put(key, list);
			}
			list.add(page);
			registeredPages.put(page, Boolean.TRUE);
		}
	}
	
	@Override
	public void registerPage(Page page) {
		if(registeredPages.get(page) == Boolean.TRUE)
			return;
		
		if(page.getParent() == null) {
			addToList(topLevelPages, page.getId(), page);
		} else {
			registerPage(page.getParent());
			addToList(subpages, page.getParent(), page);
		}
	}
	
	private void appendURL(Page page, StringBuilder sb) {
		if(page.getParent() != null) {
			appendURL(page.getParent(), sb);
		} 
		
		sb.append("/");
		
		try {
			sb.append(URLEncoder.encode(page.getId(), "UTF-8"));
		} catch (UnsupportedEncodingException e) {
			throw new RuntimeException(e);
		}
	}

	public void renderURL(String url, PrintWriter out)
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
		
		List<Page> pages = new LinkedList<Page>();
		synchronized(this) {
			// Note: Since all URLs begin with "/", ignore first component!
			List<Page> tlp = topLevelPages.get(ids[1]);
			if(tlp != null)
				pages.addAll(tlp);
			int nextIndex = 2;
			while(true) {
				if(pages.size() == 0) {
					renderError(out, url, "Id #%d (%s) yields no pages.", nextIndex - 1, ids[nextIndex - 1]);
					return;
				}
				
				if(nextIndex == ids.length)
					break;
				
				List<Page> newPages = new LinkedList<Page>();
				for(Page page : pages) {
					List<Page> sps = subpages.get(page);
					if(sps != null) {
						for(Page subpage : sps) {
							if(subpage.getId().equals(ids[nextIndex]))
								newPages.add(subpage);
						}
					}
				}
				
				pages = newPages;
				nextIndex++;
			}
		}
		
		HtmlOutput output = new HtmlOutput(this, pages, out);
		
		out.println("<HTML>");
		out.println("<HEAD>");
		
		out.println("<TITLE>");
		out.println(StringEscapeUtils.escapeHtml(url));
		out.println("</TITLE>");
		
        out.println("<STYLE>");
        out.println("DIV.log {");
        out.println("    border-width: thin;");
        out.println("    border-style: solid;");
        out.println("    margin-top: .1cm;");
        out.println("    margin-bottom: .1cm;");
        out.println("    margin-left: .3cm;                    ");
        out.println("}");
        out.println(".initiallyOpen {");
        out.println("    opacity: 1.0;                    ");
        out.println("}");
        out.println(".initiallyClosed {");
        out.println("    opacity: 0.25;");
        out.println("}");
        out.println("A:hover {");
        out.println("    text-decoration: underline;");
        out.println("}");
        out.println("A:link {");
        out.println("    text-decoration: none;");
        out.println("}");
        out.println("A:visited {");
        out.println("    text-decoration: none;");
        out.println("}");
        out.println("</STYLE>");
		
		out.println("</HEAD>");
        out.println("<BODY>");
        out.println("<DIV id='id0'>");

		for(Page page : pages) {
			page.renderInPage(output);
		}
		
        out.println("</DIV");
        out.println("</BODY>");
        out.println("</HTML>");
	}

	@Override
	public Context context() {
		return new AbstractContext(this, Arrays.asList(indexPage));
	}

	@Override
	public void addToLine(Line previousLine, Object o) {
		if(o instanceof String) {
			previousLine.addText((String)o);
		} else if (o instanceof Number) {
			previousLine.addNumber((Number)o);
		} else if (o instanceof CustomOutput) {
			previousLine.addContent((CustomOutput)o);
		} else {
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
		if(req.getRequestURI().equals("/")) {
			resp.sendRedirect(url(indexPage));
			return;
		}
		
		PrintWriter writer = resp.getWriter();
		renderURL(req.getRequestURI(), writer);
		writer.close();
	}

	@Override
	public Page getIndexPage() {
		return indexPage;
	}
	
}
