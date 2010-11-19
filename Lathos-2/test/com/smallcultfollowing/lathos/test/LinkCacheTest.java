package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;

public class LinkCacheTest
implements Page
{
    int counter = 0;
    
    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.text("Here is a fresh object foo: ");
        ConsPair foo = new ConsPair("foo" + counter++, null);
        out.obj(foo);
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        return Lathos.reflectiveDerefPage(this, link);
    }

            
    
}
