package com.smallcultfollowing.lathos;

import java.io.IOException;

public class DevNullPage
    implements ExtensiblePage, RootPage, Page.Detailed
{

    public static final DevNullPage instance = new DevNullPage();

    @Override
    public void renderObjectTitle(Output out, Link link)
    {
    }

    @Override
    public void renderDetails(Output out, Link link)
    {
    }

    @Override
    public Object derefPage(LathosServer server, String link)
    {
        return null;
    }

    @Override
    public void renderSummary(Output out, Link link)
    {
    }

    @Override
    public void addSubPage(String id, Page page)
    {
    }

    @Override
    public String rootPageName()
    {
        return "devNull";
    }

}
