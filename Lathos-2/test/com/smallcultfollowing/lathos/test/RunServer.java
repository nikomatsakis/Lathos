package com.smallcultfollowing.lathos.test;

import com.smallcultfollowing.lathos.Context;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;

public class RunServer
{
    public static void main(String[] args) throws Exception
    {
        LathosServer server = Lathos.serverOnPort(8080);
        
        // Load up the index page:
        Context ctx = server.context();
        ctx.log("Test of the index page");
        
        ReflectiveTest test0 = new ReflectiveTest("Test0"); 
        server.addRootPage(test0);
        test0.deref0 = new ReflectiveTest("Deref0");
        test0.deref1 = new ReflectiveTest("Deref1");
        
        server.join();
    }
}
