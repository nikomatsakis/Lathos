package org.rendersnake.ext.debug;

import java.io.IOException;

import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;
import org.rendersnake.RenderableWrapper;
import static org.rendersnake.AttributesFactory.*;
/**
 * Inspector can display debugging information for a component.
 * 
 * @author ernestmicklei
 *
 */
public class Inspector extends RenderableWrapper {

	public Renderable target;

	public Inspector(Renderable toInspect) {
	    super(toInspect);
		this.target = toInspect;
	}

	public static void renderCssOn(HtmlCanvas canvas) throws IOException {// @formatter:off
		canvas
			.style_css()
			.text(".rendersnake-inspector { border: 1px solid red }")
			._style();
	}
	// @formatter:on
	
	public void renderOn(HtmlCanvas canvas) throws IOException {// @formatter:off
		canvas
			.div(clazz("rendersnake-inspector"))
			.text(target.getClass().getName());
		canvas.getPageContext().renderForInpectorOn(this,canvas);
		target.renderOn(canvas);
		canvas._div();
	}
	// @formatter:on
}
