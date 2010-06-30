package com.smallcultfollowing.lathos.jetty;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.server.LathosServlet;

public class JettyLathos {
	
	public static LathosServer start(int port) throws Exception {
        Server server = new Server(8080);
			 
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        server.setHandler(context);
			 
        LathosServlet servlet = new LathosServlet();
        context.addServlet(new ServletHolder(servlet),"/*");
			 
        server.start();
        
        return servlet;
	}

}