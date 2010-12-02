package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Arrays;

/**
 * The default {@link Line} implementation. Stores its contained objects in an
 * array.
 */
public class ArrayLine
    implements Line
{
    private Object[] objs;

    public ArrayLine(Object[] objs)
    {
        this.objs = objs;
    }

    @Override
    public synchronized void addObjectsToLine(Object... moreObjs)
    {
        Object[] newObjs = Arrays.copyOf(objs, objs.length + moreObjs.length);
        System.arraycopy(moreObjs, 0, newObjs, objs.length, moreObjs.length);
        objs = newObjs;

    }

    @Override
    public synchronized void renderSummary(Output out, Link link) throws IOException
    {
        for (int i = 0, c = objs.length; i < c; i++) {
            out.obj(new IndexLink(link, i), objs[i]);
        }
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        int i = IndexLink.parseIndexLink(link);
        if (i >= 0 && i < objs.length)
            return objs[i];
        throw InvalidDeref.instance;
    }

}
