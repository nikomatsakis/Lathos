package com.smallcultfollowing.lathos.test;

import com.smallcultfollowing.lathos.Context;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Line;

public class RunServer
{
    public static void main(String[] args) throws Exception
    {
        LathosServer server = Lathos.serverOnPort(8080);
        
        TestSubst testSubst = new TestSubst("subst-from", "subst-to");
        server.addSubstitutionFilter(testSubst);
        
        // Load up the index page:
        Context ctx = server.context();
        ctx.log("Test of the index page");
        
        ReflectiveTest test0 = new ReflectiveTest("Test0"); 
        server.addRootPage(test0);
        test0.deref0 = new ReflectiveTest("Deref0");
        test0.deref1 = new ReflectiveTest("Deref1");
        
        Line line = ctx.log("Second line, with test0: ", test0);
        ctx.log("Third line, deref0: ", test0.deref0);
        line.addObjectsToLine(" deref1: ", test0.deref1);
        
        ctx.log("Testing substitution (should say subst-to): ", "subst-from");
        
        ConsPair d1 = new ConsPair("d1", new ConsPair("d2", null));
        ctx.log("This object, ", d1, " is not modified in any way.  It should be clickable though.");

        ctx.log("LinkCacheTest: ", new LinkCacheTest());
        
        ctx.log("Don't forget to try the url /static/", StaticTest.class.getName());
        
        server.join();
    }
}
