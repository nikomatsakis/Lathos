package com.smallcultfollowing.lathos.test;

import java.io.IOException;

import com.smallcultfollowing.lathos.context.Context;
import com.smallcultfollowing.lathos.jetty.JettyLathos;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.PageContent;
import com.smallcultfollowing.lathos.model.Pages;

public class JustOnePage {
	
	public static class CustomData1 implements Page {
		
		public final int i;
		
		public CustomData1(int i) {
			super();
			this.i = i;
		}

		@Override
		public void renderInPage(Output out) throws IOException {
			out.startDiv();
			
			out.startPar();
			out.startBold();
			out.outputText("CustomData1");
			out.endBold();
			out.endPar();
			
			out.startPar();
			out.outputText("i = ");
			out.outputText(Integer.toString(i));
			out.endPar();
			
			out.endDiv();
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			Pages.renderInLine(this, output);
		}

		@Override
		public String getId() {
			return String.format(
					"custom1(%x)", 
					System.identityHashCode(this));
		}

		@Override
		public Page getParent() {
			return null;
		}

		@Override
		public void addContent(PageContent content) {
			throw new UnsupportedOperationException();
		}
		
	}
	
	public static void main(String[] args)
	throws Exception
	{
		
		LathosServer server = JettyLathos.start(8080);
		Context ctx = server.context();
		
		Page topPage1 = makeTopPage(ctx);
		Page topPage2 = makeTopPage(ctx);
		
		Page bar = makeChildPage(ctx, "Bar");
		ctx.log("You can view other ", 
				ctx.link(topPage1, "top"), " ", ctx.link(topPage2, "pages"), 
				", or we could ", ctx.link(bar, "link ", "directly to bar"));		
		
		ctx.push(topPage1);
		Page foo = makeChildPage(ctx, "Foo");
		ctx.log("This log entry was added after ", foo);
		ctx.pop(topPage1);
	}

	private static Page makeChildPage(Context ctx, String name) {
		Page foo = ctx.pushChild(name, "This is a subpage, ", name);
		ctx.log(name, " has some log items.");
		ctx.pop(foo);
		return foo;
	}

	private static Page makeTopPage(Context ctx) {
		Page topPage = ctx.pushTopLevel("topPage", "This", " is", " a", " test.");
		
		for(int i = 0; i < 3; i++) {
			ctx.log("i = ", i, " custom = ", new CustomData1(i));
		}
		
		ctx.pop(topPage);
		
		return topPage;
	}

}
