package com.smallcultfollowing.lathos.http;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class LathosServlet extends HttpServlet {
	private static final long serialVersionUID = 7921875518166654758L;
	private final HttpLathosServer server;
	
	public LathosServlet(HttpLathosServer server) {
		super();
		this.server = server;
	}

	@Override
	protected void doGet(HttpServletRequest req, HttpServletResponse resp)
			throws ServletException, IOException 
	{
		if(req.getRequestURI().equals("/")) {
			resp.sendRedirect(server.url(server.getIndexPage()));
			return;
		}
		
		PrintWriter writer = resp.getWriter();
		server.renderURL(req.getRequestURI(), writer);
		writer.close();
	}
}
