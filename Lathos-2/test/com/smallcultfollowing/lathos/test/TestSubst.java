package com.smallcultfollowing.lathos.test;

import com.smallcultfollowing.lathos.ObjectSubst;

public class TestSubst
    implements ObjectSubst
{

    public final Object from, to;

    public TestSubst(Object from, Object to)
    {
        super();
        this.from = from;
        this.to = to;
    }

    @Override
    public Object substitute(Object obj)
    {
        if (obj.equals(from))
            return to;
        return obj;
    }

}
