package com.smallcultfollowing.lathos;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;


public class JettyLathosServer extends HttpLathosServer {
	private static final long serialVersionUID = -6551558762231802620L;
	private final Server jettyServer;
	
	public JettyLathosServer(int port) throws Exception {
		jettyServer = new Server(8080);
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        jettyServer.setHandler(context);
        context.addServlet(new ServletHolder(new LathosServlet(this)), "/*");
        jettyServer.start();
	}
	
	public static JettyLathosServer start(int port) throws Exception {
		return new JettyLathosServer(port);
	}
	
	public static LathosServer startConditionally(int port) throws Exception {
		if(System.getProperty("LATHOS") == null)
			return new NoneServer();
		
		return start(port);
	}
	
	public void join() {
		try {
			jettyServer.join();
		} catch (InterruptedException e) {
		}
	}

}