package com.smallcultfollowing.lathos.model;

import com.smallcultfollowing.lathos.context.Context;

public interface LathosServer {

	public void addTopLevelPage(Page page);
	
	public String freshId();
	
	public Context context();

	public void addToLine(Line line, Object o);
	
}
