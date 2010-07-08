package com.smallcultfollowing.lathos.model;

import java.io.IOException;
import java.lang.reflect.Field;
import java.util.Arrays;

/** Utility methods for {@link Page} implementations */
public class Util {

	public static void renderInLine(Page page, Output output) 
	throws IOException {
		output.startLink(page);
		output.outputText(page.toString());
		output.endLink(page);
	}
	
	public static void headerRow(Output output, Object... labels) throws IOException {
		output.startRow();
		for(Object label : labels) {
			output.startColumn();
			output.startBold();
			output.outputObject(label);
			output.endBold();
			output.endColumn();
		}
		output.endRow();
	}
	
	public static void row(Output output, Object... columns)
	throws IOException {
		output.startRow();

		for(Object column : columns) {
			output.startColumn();
			output.outputObject(column);
			output.endColumn();
		}
		
		output.endRow();
	}
	
	public static void reflectivePage(Page page, Output output)
	throws IOException
	{
		output.startPage(page);
		
		output.startPar();
		output.startBold();
		output.outputText(page.toString());
		output.endBold();
		output.endPar();
		
		output.startTable();
		
		Util.headerRow(output, "Field", "Value");
		
		for(Class<?> cls = page.getClass(); cls != null; cls = cls.getSuperclass()) {
			for(Field fld : cls.getDeclaredFields()) {
				fld.setAccessible(true);
				output.startRow();
				
				output.startColumn();
				output.outputText(fld.getName());
				output.endColumn();

				try {
					output.startColumn();
					output.outputObject(fld.get(page));
					output.endColumn();
				} catch (Exception e) {
					output.outputText("Failed to access: ");
					output.outputObject(e);
				}
				
				output.endRow();
			}
		}
		output.endTable();
		
		output.endPage(page);
	}

}
