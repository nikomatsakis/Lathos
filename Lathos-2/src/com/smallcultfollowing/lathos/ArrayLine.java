package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Arrays;

public class ArrayLine
    implements Line
{
    private final LathosServer server;
    private Object[] objs;
    
    public ArrayLine(LathosServer server, Object[] objs)
    {
        this.server = server;
        this.objs = objs;
        for(int i = 0; i < objs.length; i++) {
            objs[i] = server.substitute(objs[i]);
        }
    }

    @Override
    public synchronized void addObjectsToLine(Object... moreObjs)
    {
        Object[] newObjs = Arrays.copyOf(objs, objs.length + moreObjs.length);
        System.arraycopy(moreObjs, 0, newObjs, objs.length, moreObjs.length);
        for(int i = objs.length; i < newObjs.length; i++) {
            newObjs[i] = server.substitute(newObjs[i]);
        }
        objs = newObjs;
        
    }

    @Override
    public synchronized void renderSummary(Output out, Link link) throws IOException
    {
        for(int i = 0, c = objs.length; i < c; i++) {
            out.obj(new IndexLink(link, i), objs[i]);            
        }
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        renderSummary(out, link);
    }

    @Override
    public Object derefPage(String link)
    {
        try {
            int i = Integer.parseInt(link);
            if(i >= 0 && i < objs.length)
                return objs[i];
            return null;            
        } catch (NumberFormatException err){
            return null;
        }
    }

}
