package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;

public class StaticTest
implements Page
{
    public static String stringField = "foo";
    public static int intField = 10;
    public static StaticTest instance = new StaticTest();
    
    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }
    
    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.h1().text("Hello world!")._h1();
        Lathos.reflectiveRenderDetails(this, out, link);
    }
    
    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        return Lathos.reflectiveDerefPage(this, link);
    }
}
