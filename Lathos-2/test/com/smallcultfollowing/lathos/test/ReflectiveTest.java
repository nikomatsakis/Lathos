package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.Ignore;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.RootPage;

public class ReflectiveTest
implements RootPage
{
    private final String name;
    
    @SuppressWarnings("unused")
    @Ignore 
    private final int ignoreMe = 22;
    
    public ReflectiveTest deref0;
    
    public ReflectiveTest deref1;
    
    public ReflectiveTest(String name)
    {
        super();
        this.name = name;
    }

    @Override
    public void renderAsLine(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderAsLine(this, out, link);
    }

    @Override
    public void renderAsPage(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderAsPage(this, out, link);        
    }

    @Override
    public Object derefPage(String link)
    {
        return Lathos.reflectiveDerefPage(this, link);
    }

    @Override
    public String rootPageName()
    {
        return name;
    }
    
    @Override
    public String toString()
    {
        return name;
    }

}
