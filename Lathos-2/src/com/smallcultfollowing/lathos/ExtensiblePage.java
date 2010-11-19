package com.smallcultfollowing.lathos;

public interface ExtensiblePage
    extends Page
{
    public void addSubPage(String link, Page page);
}
