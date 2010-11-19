package com.smallcultfollowing.lathos;

public class DevNullLine
    implements Line
{
    public static final DevNullLine instance = new DevNullLine();

    @Override
    public void renderSummary(Output out, Link link)
    {
    }

    @Override
    public void addObjectsToLine(Object... objs)
    {
    }

    @Override
    public void renderDetails(Output out, Link link)
    {
    }

    @Override
    public Object derefPage(String link)
    {
        return this;
    }

}
