package org.rendersnake.ext.jquery;

import java.io.IOException;

import org.rendersnake.HtmlCanvas;

public class JQueryLibrary {

	/** 
	 * Write a link reference to load the external Javascript library for JQuery.
	 * This should be part of the HEAD section of an HTML page.
	 * 
	 * @param canvas
	 */
	public void renderLoadCoreOn(String version, HtmlCanvas canvas) throws IOException {
	    // http://code.google.com/apis/libraries/devguide.html#jquery
	    //
		// <script src="http://ajax.googleapis.com/ajax/libs/jquery/1.4.2/jquery.min.js" type="text/javascript"></script>
	    
	    canvas.script_javascript("http://ajax.googleapis.com/ajax/libs/jquery/"+version+"/jquery.min.js");
	}
    public void renderLoadUIOn(String version, HtmlCanvas canvas) throws IOException {
        // http://code.google.com/apis/libraries/devguide.html#jqueryUI
        //
        // http://ajax.googleapis.com/ajax/libs/jqueryui/1.8.5/jquery-ui.min.js
        
        canvas.script_javascript("http://ajax.googleapis.com/ajax/libs/jqueryui/"+version+"/jquery-ui.min.js");
    }
}
