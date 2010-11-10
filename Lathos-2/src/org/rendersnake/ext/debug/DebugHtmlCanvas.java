package org.rendersnake.ext.debug;

import java.io.IOException;
import java.io.Writer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.rendersnake.Attributes;
import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;
/**
 * DebugHtmlCanvas is a special HtmlCanvas that can render inspector wrappers around component on a HTML page.
 * Its purpose is to reveal the structure of a page by displaying debugging information.
 * 
 * @author ernestmicklei
 */
public class DebugHtmlCanvas extends HtmlCanvas {

	public boolean enabled = true;
	public int renderCount = 0;

	public DebugHtmlCanvas() {
		super();
	}

	public DebugHtmlCanvas(HttpServletRequest request,
			HttpServletResponse response, Writer out) {
		super(request, response, out);
	}

	public DebugHtmlCanvas(Writer output) {
		super(output);
	}

	@Override
	public HtmlCanvas render(Renderable component) throws IOException {
		renderCount++;
		if (enabled)
			return super.render(new Inspector(component));
		else
			return super.render(component);
	}

	@Override
	public HtmlCanvas body() throws IOException {
		super.body();
		return this.renderInspectorCss();
	}

	@Override
	public HtmlCanvas body(Attributes attrs) throws IOException {
		super.body(attrs);
		return this.renderInspectorCss();
	}

	private HtmlCanvas renderInspectorCss() throws IOException {
		Inspector.renderCssOn(this);
		return this;
	}
}
