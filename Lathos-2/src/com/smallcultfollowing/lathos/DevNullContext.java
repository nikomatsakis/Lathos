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
        return page;
    }

    @Override
    public void pop(ExtensiblePage page)
    {
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
    public ExtensiblePage newPage(String name, Object... title)
    {
        return DevNullPage.instance;
    }

    @Override
    public Page linked(Object linkTo, Object... text)
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

    @Override
    public void append(Line line, Object... objs)
    {
    }

    @Override
    public Page i18n(String fmt, Object... args)
    {
        return null;
    }
}
