package com.smallcultfollowing.lathos;



public interface Page 
extends PageContent, CustomOutput
{
	public String getId();
	public Page getParent();
	public void addContent(PageContent content);
}
