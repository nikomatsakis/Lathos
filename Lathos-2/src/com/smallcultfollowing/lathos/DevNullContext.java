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
    public ExtensiblePage subpage(String name)
    {
        return DevNullPage.instance;
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
}
