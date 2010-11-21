package com.smallcultfollowing.lathos;

import java.io.IOException;

/**
 * The {@code LathosServerDelegate} allows for some
 * simple configuration of the "look and feel" of a 
 * Lathos web site.
 */
public interface LathosServerDelegate
{
    public boolean handleRequest(String url, Output out) throws IOException;
    public void startHtmlPage(Output out, Link link, Object rootPage) throws IOException;
    public void endHtmlPage(Output out, Link link, Object rootPage) throws IOException;
    public void startEmbed(Output out, int embedDepth, Link link, Object obj) throws IOException;
    public void endEmbed(Output out, int embedDepth, Link link, Object obj) throws IOException;
}
