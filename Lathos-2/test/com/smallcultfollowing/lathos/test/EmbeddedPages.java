package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;

public class EmbeddedPages
    implements Page.Detailed, Page.Titled
{
    ConsPair lst1 = new ConsPair("foo", new ConsPair("bar", null));
    ConsPair lst2 = new ConsPair("a", new ConsPair("b", new ConsPair("c", null)));

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderTitle(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderTitle(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.embed(link, "lst1", lst1);
        out.embed(link, "lst2", lst2);

        out.subpage();
        out.text("A subpage!");
        out.subpage();
        out.text("A nested subpage!");
        out._subpage();
        out.text("Back in the original subpage!");
        out._subpage();
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        return Lathos.reflectiveDerefPage(this, link);
    }

}
