package org.rendersnake;

import java.io.IOException;
/**
 * 
 * @see http://www.w3schools.com/tags/tag_DOCTYPE.asp
 * @author e.micklei
 */
public enum DocType implements Renderable {
	XHTML_1_0_Strict ("<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Strict//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd\">"),
	HTML_4_01_Strict ("<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.01//EN\" \"http://www.w3.org/TR/html4/strict.dtd\">");

	private final String declaration;
	DocType(String declaration) {
		this.declaration = declaration;
	}
	public void renderOn(HtmlCanvas canvas) throws IOException {
		canvas.text(this.declaration);
	}
}
