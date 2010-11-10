package org.rendersnake.ext.jquery;

import java.io.StringWriter;

/**
 * JQueryCanvas can write jQuery expressions.
 * Use {@link toJavascript} to pass the expression String to an Html attribute. 
 * 
 * @author ernestmicklei
 */
public class JQueryCanvas implements ToJavascript {

	private StringWriter out = new StringWriter();

	public JQueryCanvas jQuery(String selector) {
	    out.write("$('");
	    out.write(selector);
	    out.write("')");
	    return this;
	}
	public JQueryCanvas jQuery(String selector,String context) {
	    out.write("$('");
	    out.write(selector);
	    out.write("',");
	    out.write(context);
	    out.write(')');
	    return this;
	}	
    public JQueryCanvas _jQuery() {
        out.write(';');
        return this;
    }		
	public String toJavascript() {
		return out.toString();
	}
    
    ///////////////////////////////////////////////////////////////////////////////
    //
    //  Methods below are generated using XSLT (see /html-codegen). DO NOT EDIT
    //   
    ///////////////////////////////////////////////////////////////////////////////  


    public JQueryCanvas addClass(String cssClass) {
        out.write(".addClass('");
        out.write(cssClass);
        out.write("')");
        return this;
    }
}
