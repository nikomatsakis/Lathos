package com.smallcultfollowing.lathos.test;

import java.io.IOException;
import java.util.List;

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
			
			out.startLine();
			out.startBold();
			out.outputText("CustomData1");
			out.endBold();
			out.endLine();
			
			out.startLine();
			out.outputText("i = ");
			out.outputText(Integer.toString(i));
			out.endLine();
			
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

		@Override
		public void addSubpages(String withId, List<Page> toList) {
		}
		
	}
	
	public static void main(String[] args)
	throws Exception
	{
		
		LathosServer server = JettyLathos.start(8080);
		Context ctx = server.context();
		
		Page index1 = makeIndex(ctx);
		makeIndex(ctx);
		
		ctx.push(index1);
		Page foo = ctx.pushChild("Foo", "This is a subpage, Foo");
		ctx.log("Foo has some log items.");
		ctx.pop(foo);
		ctx.pop(index1);
	}

	private static Page makeIndex(Context ctx) {
		Page index = ctx.pushTopLevel("index", "This", " is", " a", " test.");
		
		for(int i = 0; i < 3; i++) {
			ctx.log("i = ", i, " custom = ", new CustomData1(i));
		}
		
		ctx.pop(index);
		
		return index;
	}

}
