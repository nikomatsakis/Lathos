package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;

public class LinkCacheTest
implements Page
{
    int counter = 0;
    
    @Override
    public void renderAsLine(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderAsLine(this, out, link);
    }

    @Override
    public void renderAsPage(Output out, Link link) throws IOException
    {
        out.text("Here is a fresh object foo: ");
        ConsPair foo = new ConsPair("foo" + counter++, null);
        out.renderObject(foo);
    }

    @Override
    public Object derefPage(String link)
    {
        return Lathos.reflectiveDerefPage(this, link);
    }

            
    
}
