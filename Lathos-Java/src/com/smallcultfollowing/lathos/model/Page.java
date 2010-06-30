package com.smallcultfollowing.lathos.model;

import java.util.List;


public interface Page 
extends PageContent, CustomOutput
{
	public String getId();
	public Page getParent();
	public void addSubpages(String withId, List<Page> toList);
	public void addContent(PageContent content);
}
