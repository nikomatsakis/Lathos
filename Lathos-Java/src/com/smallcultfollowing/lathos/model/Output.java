package com.smallcultfollowing.lathos.model;

import java.io.IOException;
import java.util.List;;

public interface Output {

	public List<Page> topPages();

	public void outputObject(Object content)
	throws IOException;
	
	public void outputText(String content)
	throws IOException;
	
	public void startLink(Page target)
	throws IOException;

	public void endLink(Page target)
	throws IOException;

	public String startDiv()
	throws IOException;

	public void endDiv()
	throws IOException;

	public void startLine()
	throws IOException;
	
	public void endLine()
	throws IOException;

	public void startBold()
	throws IOException;
	
	public void endBold()
	throws IOException;

}
