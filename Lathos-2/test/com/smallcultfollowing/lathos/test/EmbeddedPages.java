package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;

public class EmbeddedPages
implements Page
{
    ConsPair lst1 = new ConsPair("foo", new ConsPair("bar", null));
    ConsPair lst2 = new ConsPair("a", new ConsPair("b", new ConsPair("c", null)));
    
    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.embed(link, "lst1", lst1);
        out.embed(link, "lst2", lst2);
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        return Lathos.reflectiveDerefPage(this, link);
    }
    
}
