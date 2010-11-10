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
}
