package com.smallcultfollowing.lathos;

public class DevNullPage
    implements ExtensiblePage, RootPage
{

    public static final DevNullPage instance = new DevNullPage();

    @Override
    public void renderAsPage(Output out, Link link)
    {
    }

    @Override
    public Object derefPage(String link)
    {
        return null;
    }

    @Override
    public void renderAsLine(Output out, Link link)
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
