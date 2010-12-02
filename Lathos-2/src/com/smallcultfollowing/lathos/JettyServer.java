package com.smallcultfollowing.lathos;

import java.io.IOException;

import org.eclipse.jetty.server.Server;
import org.eclipse.jetty.servlet.ServletContextHandler;
import org.eclipse.jetty.servlet.ServletHolder;

public class JettyServer
    extends DefaultServer
{
    private final int port;
    private final Server jettyServer;

    public JettyServer(int port) throws Exception
    {
        this.port = port;
        jettyServer = new Server(port);
        ServletContextHandler context = new ServletContextHandler(ServletContextHandler.SESSIONS);
        context.setContextPath("/");
        jettyServer.setHandler(context);
        context.addServlet(new ServletHolder(this), "/*");
        jettyServer.start();
    }

    public static LathosServer start(int port) throws Exception
    {
        if (port == 0)
            return DevNullServer.instance;
        return new JettyServer(port);
    }

    @Override
    public void join()
    {
        try {
            jettyServer.join();
        } catch (InterruptedException e) {
        }
    }

    @Override
    public void stop() throws Exception
    {
        jettyServer.stop();
    }

    @Override
    public void openInBrowser()
    {
        String url = String.format("http://localhost:%d/", port);
        try {
            java.awt.Desktop.getDesktop().browse(java.net.URI.create(url));
        } catch (IOException e) {
        }
    }
}
