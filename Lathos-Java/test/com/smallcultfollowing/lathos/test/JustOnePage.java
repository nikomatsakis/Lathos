package com.smallcultfollowing.lathos.test;

import com.smallcultfollowing.lathos.context.Context;
import com.smallcultfollowing.lathos.jetty.JettyLathos;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Page;

public class JustOnePage {
	
	public static void main(String[] args)
	throws Exception
	{
		
		LathosServer server = JettyLathos.start(8080);
		Context ctx = server.context();
		
		Page index = ctx.pushTopLevel("index", "This", " is", " a", " test.");
		
		for(int i = 0; i < 22; i++) {
			ctx.log("i = ", i);
		}
		
		ctx.pop(index);
		
	}

}