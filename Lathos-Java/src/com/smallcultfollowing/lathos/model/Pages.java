package com.smallcultfollowing.lathos.model;

import java.io.IOException;

/** Utility methods for {@link Page} implementations */
public class Pages {

	public static void renderInLine(Page page, Output output) 
	throws IOException {
		output.startLink(page);
		output.outputText(page.getId());
		output.endLink(page);
	}

}
