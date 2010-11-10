package com.smallcultfollowing.lathos;

public class DevNullLine
    implements Line
{
    public static final DevNullLine instance = new DevNullLine();

    @Override
    public void renderAsLine(Output out, Link link)
    {
    }

    @Override
    public void addObjectsToLine(Object... objs)
    {
    }

    @Override
    public void renderAsPage(Output out, Link link)
    {
    }

    @Override
    public Object derefPage(String link)
    {
        return this;
    }

}
