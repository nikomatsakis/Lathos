package com.smallcultfollowing.lathos.model;

import java.io.IOException;

/** Utility methods for {@link Page} implementations */
public class Util {

	public static void renderInLine(Page page, Output output) 
	throws IOException {
		output.startLink(page);
		output.outputText(page.getId());
		output.endLink(page);
	}
	
	public static void row(Output output, Object... columns)
	throws IOException {
		output.startRow();

		for(Object column : columns) {
			output.startColumn();
			output.outputText(column.toString());
			output.endColumn();
		}
		
		output.endRow();
	}
	
}
