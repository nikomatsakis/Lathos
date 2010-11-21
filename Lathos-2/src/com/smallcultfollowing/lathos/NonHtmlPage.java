package com.smallcultfollowing.lathos;

import java.io.IOException;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

/**
 * If an object implements this interface, then when it is referenced directly
 * via a URL, the {@code renderNonHtml()} method will be invoked by the lathos
 * server.
 */
public interface NonHtmlPage
{
    public void renderNonHtml(HttpServletRequest request, HttpServletResponse response, Link link) throws IOException;
}
