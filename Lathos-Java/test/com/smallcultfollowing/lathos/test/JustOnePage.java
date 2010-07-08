package com.smallcultfollowing.lathos.test;

import java.io.IOException;
import java.util.Date;

import com.smallcultfollowing.lathos.http.JettyLathosServer;
import com.smallcultfollowing.lathos.model.Context;
import com.smallcultfollowing.lathos.model.CustomOutput;
import com.smallcultfollowing.lathos.model.DataRenderer;
import com.smallcultfollowing.lathos.model.LathosServer;
import com.smallcultfollowing.lathos.model.Line;
import com.smallcultfollowing.lathos.model.Output;
import com.smallcultfollowing.lathos.model.Page;
import com.smallcultfollowing.lathos.model.PageContent;
import com.smallcultfollowing.lathos.model.ThrowableDataRenderer;
import com.smallcultfollowing.lathos.model.Util;

public class JustOnePage {
	
	public static class CustomData1 implements Page {
		
		public final int i;
		
		public CustomData1(int i) {
			super();
			this.i = i;
		}

		@Override
		public void renderInPage(Output out) throws IOException {
			Util.reflectivePage(this, out);
		}

		@Override
		public void renderInLine(Output output) throws IOException {
			Util.renderInLine(this, output);
		}
		
		@Override
		public String toString() {
			return getId();
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
	
	public static class SomeClass {
		public final String name;
		public final String details;
		
		public SomeClass(String name, String details) {
			super();
			this.name = name;
			this.details = details;
		}
	}
	
	public static class Renderer implements DataRenderer {
		
		class RenderSomeClass implements CustomOutput, Page {
			
			public final SomeClass renderable;

			public RenderSomeClass(SomeClass renderable) {
				super();
				this.renderable = renderable;
			}

			@Override
			public void renderInLine(Output output) throws IOException {
				output.outputText("[");
				output.startLink(this);
				output.outputText("SomeClass ");
				output.endLink(this);
				output.outputText("name=");
				output.outputText(renderable.name);
				output.outputText("]");
			}

			@Override
			public void renderInPage(Output output) throws IOException {
				output.startPage(this);
				
				output.startPar();
				output.startBold();
				output.outputText(renderable.name);
				output.endBold();
				output.endPar();
				
				output.startPar();
				output.outputText(renderable.details);
				output.endPar();
				
				output.endPage(this);
			}

			@Override
			public String getId() {
				return "renderable["+renderable.name+"]";
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

		@Override
		public boolean addToLine(Line line, Object object) {
			if(object instanceof SomeClass) {
				line.addContent(new RenderSomeClass((SomeClass) object));
				return true;
			}
			return false;
		}
		
	}
	
	public static void main(String[] args)
	throws Exception
	{
		LathosServer server = JettyLathosServer.start(8080);
		server.addDataRenderer(new ThrowableDataRenderer());
		Context ctx = server.context();

		// Test constructing top-level pages:
		Page topPage1 = makeTopPage(ctx);
		Page topPage2 = makeTopPage(ctx);
		
		// Test child pages:
		Page bar = makeChildPage(ctx, "Bar");
		ctx.log("You can view other ", 
				ctx.link(topPage1, "top"), " ", ctx.link(topPage2, "pages"), 
				", or we could ", ctx.link(bar, "link ", "directly to bar"));
		
		// Test pages of custom type:
		ctx.log("Subtypes of page are easy to embed: ", new CustomData1(22));
		
		// Test custom rendering::
		SomeClass renderable1 = new SomeClass("Albert Einstein", "One smart dude.");	
		SomeClass renderable2 = new SomeClass("George Bush", "Not so much.");
		ctx.log("Custom objects can be rendered: ", renderable1, ", ", renderable2);
		server.addDataRenderer(new Renderer());
		ctx.log("but only after adding a custom renderer: ", renderable1, ", ", renderable2);

		// Test fall back to string:
		ctx.log("Unknown objects just use toString: ", new Date());
		
		// Test the built-in throwable render:
		UnsupportedOperationException exc = new UnsupportedOperationException(new ClassCastException());
		ctx.log("Throwable objects have built-in support: ", exc);

		// Test pushing an unrelated page and adding to it later:
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
