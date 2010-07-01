package com.smallcultfollowing.lathos.model;



public interface Page 
extends PageContent, CustomOutput
{
	public String getId();
	public Page getParent();
	public void addContent(PageContent content);
}
