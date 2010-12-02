package com.smallcultfollowing.lathos;

@SuppressWarnings("serial")
public class PoppedWrongPageException
    extends RuntimeException
{
    public final Page popped;
    public final Page expected;

    public PoppedWrongPageException(Page popped, Page expected)
    {
        super();
        this.popped = popped;
        this.expected = expected;
    }
}
