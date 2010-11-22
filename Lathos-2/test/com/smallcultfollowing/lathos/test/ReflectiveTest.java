package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.AllowReflectiveDeref;
import com.smallcultfollowing.lathos.Ignore;
import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
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
