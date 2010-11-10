package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.List;;

public interface Output {

	public List<Page> topPages();

	public void outputText(String content)
	throws IOException;
	
	public void outputObject(Object content)
	throws IOException;
	
	public void startLink(Page target)
	throws IOException;

	public void endLink(Page target)
	throws IOException;

	/** Starts a sub-page or division of a page.
	 *  {@code page} may be {@code null}! */
	public String startPage(Page page)
	throws IOException;

	public void endPage(Page page)
	throws IOException;

	public void startPar()
	throws IOException;
	
	public void endPar()
	throws IOException;

	public void startPre()
	throws IOException;
	
	public void endPre()
	throws IOException;

	public void startBold()
	throws IOException;
	
	public void endBold()
	throws IOException;

	public void startTable()
	throws IOException;
	
	public void startRow()
	throws IOException;
	
	public void startColumn()
	throws IOException;
	
	public void endColumn()
	throws IOException;
	
	public void endRow()
	throws IOException;
	
	public void endTable()
	throws IOException;
	

}
