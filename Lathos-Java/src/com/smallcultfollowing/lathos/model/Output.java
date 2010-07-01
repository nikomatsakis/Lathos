package com.smallcultfollowing.lathos.model;

import java.io.IOException;
import java.util.List;;

public interface Output {

	public List<Page> topPages();

	public void outputText(String content)
	throws IOException;
	
	public void startLink(Page target)
	throws IOException;

	public void endLink(Page target)
	throws IOException;

	public String startPage(Page page)
	throws IOException;

	public void endPage(Page page)
	throws IOException;

	public void startPar()
	throws IOException;
	
	public void endPar()
	throws IOException;

	public void startBold()
	throws IOException;
	
	public void endBold()
	throws IOException;

}
