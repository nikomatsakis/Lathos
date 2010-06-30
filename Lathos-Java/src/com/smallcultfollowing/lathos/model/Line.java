package com.smallcultfollowing.lathos.model;

public interface Line extends PageContent {
	public void addContent(CustomOutput content);
	public void addText(String text);
	public void addNumber(Number num);
}