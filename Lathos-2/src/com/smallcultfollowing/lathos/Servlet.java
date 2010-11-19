package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.PrintWriter;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public class Servlet extends HttpServlet {
    private static final long serialVersionUID = 7921875518166654758L;
    private final LathosServer server;
    
    public Servlet(LathosServer server) {
        super();
        this.server = server;
    }

    @Override
    protected void doGet(HttpServletRequest req, HttpServletResponse resp)
            throws ServletException, IOException 
    {
        if(req.getRequestURI().equals("/")) {
            Link link = new BaseLink(server.getIndexPage());
            StringBuilder sb = new StringBuilder();
            link.appendUrlString(sb);
            resp.sendRedirect("/" + sb.toString());
            return;
        }
        
        PrintWriter writer = resp.getWriter();
        server.renderURL(req.getRequestURI(), writer);
        writer.close();
    }
}
