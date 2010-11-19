package com.smallcultfollowing.lathos.test;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

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
        
        Object[] aShortArray = new Object[] { 1, d1 };
        Object[] aLongArray = new Object[] { 1, 2, 3, d1, 5, 6, 7 };
        
        List<Object> aList = new ArrayList<Object>();
        aList.add("Some");
        aList.add("Entries");
        aList.add(d1);
        
        Map<String, Object> aMap = new LinkedHashMap<String, Object>();
        aMap.put("Apples", "Oranges");
        aMap.put("Bananas", "Cocunuts");
        aMap.put("ConsPair", d1);
        aMap.put("Pumpkins", "Squash");
        
        ctx.log("Java collections should behave well.");
        ctx.log("Short array: ", aShortArray);
        ctx.log("Long array: ", aLongArray);
        ctx.log("Lists: ", aList);
        ctx.log("Maps: ", aMap);

        ctx.log("LinkCacheTest: ", new LinkCacheTest());
        
        ctx.log("Don't forget to try the url /static/", StaticTest.class.getName());
        
        server.join();
    }
}
