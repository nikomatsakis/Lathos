package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.*;

public class ReflectiveTest
implements RootPage, Page.Detailed
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

    @AllowReflectiveDeref(showInDetails = true)
    public ReflectiveTest getDeref0() {
        return deref0;
    }
    
    @AllowReflectiveDeref(showInDetails = false)
    public ReflectiveTest getDeref0X() {
        return deref0;
    }
    
    public ReflectiveTest getDeref1() {
        return deref1;
    }
    
    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderObjectTitle(Output out, Link link) throws IOException {
        Lathos.reflectiveRenderTitle(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderDetails(this, out, link);        
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
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
