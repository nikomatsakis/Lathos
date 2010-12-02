package com.smallcultfollowing.lathos;

public class DevNullPage
    implements ExtensiblePage, RootPage, Page.Detailed, Page.Titled
{

    public static final DevNullPage instance = new DevNullPage();

    @Override
    public void renderTitle(Output out, Link link)
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
