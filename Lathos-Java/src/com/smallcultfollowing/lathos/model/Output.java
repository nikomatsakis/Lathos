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


}
