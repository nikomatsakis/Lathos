package com.smallcultfollowing.lathos;

public class DevNullContext
    implements Context
{
    public static final DevNullContext instance = new DevNullContext();

    @Override
    public Line log(Object... objs)
    {
        return DevNullLine.instance;
    }

    @Override
    public ExtensiblePage push(ExtensiblePage page)
    {
        return DevNullPage.instance;
    }

    @Override
    public void pop(ExtensiblePage page)
    {
        if(page != null && page != DevNullPage.instance)
            throw new PoppedWrongPageException(DevNullPage.instance, page);
    }

    @Override
    public void embed(Page page)
    {
    }

    @Override
    public void embed(String link, Page page)
    {
    }

    @Override
    public ExtensiblePage newPage(String name)
    {
        return DevNullPage.instance;
    }

    @Override
    public Object linked(Object linkTo, Object... text)
    {
        return null;
    }

    @Override
    public LathosServer server()
    {
        return DevNullServer.instance;
    }

    @Override
    public ExtensiblePage topPage()
    {
        return DevNullPage.instance;
    }
}
