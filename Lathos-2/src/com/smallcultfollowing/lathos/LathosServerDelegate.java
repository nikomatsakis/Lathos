package com.smallcultfollowing.lathos;

import java.io.IOException;

import com.smallcultfollowing.lathos.Page.Titled;

/**
 * The {@code LathosServerDelegate} allows for some simple configuration of the
 * "look and feel" of a Lathos web site.
 */
public interface LathosServerDelegate
{
    public boolean handleRequest(String url, Output out) throws IOException;

    public void startHtmlPage(Output out, Link link, Object rootPage) throws IOException;

    public void endHtmlPage(Output out, Link link, Object rootPage) throws IOException;

    public void startEmbed(Output out, int embedDepth, Link link, Page page) throws IOException;

    public void startEmbedTitle(Output out, int embedDepth, Link link, Titled page) throws IOException;

    public void endEmbedTitle(Output out, int embedDepth, Link link, Titled page) throws IOException;

    public void startEmbedContent(Output out, int embedDepth, Link link, Page page) throws IOException;

    public void endEmbedContent(Output out, int embedDepth, Link link, Page page) throws IOException;

    public void endEmbed(Output out, int embedDepth, Link link, Page page) throws IOException;

    public void startSubpage(Output out, int embedDepth) throws IOException;

    public void startSubpageTitle(Output out, int embedDepth) throws IOException;

    public void endSubpageTitle(Output out, int embedDepth) throws IOException;

    public void startSubpageContent(Output out, int embedDepth) throws IOException;

    public void endSubpageContent(Output out, int embedDepth) throws IOException;

    public void endSubpage(Output out, int embedDepth) throws IOException;
}
