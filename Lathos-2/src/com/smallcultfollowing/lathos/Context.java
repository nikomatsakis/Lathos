package com.smallcultfollowing.lathos;

public interface Context
{
    Line log(Object... objs);
    public ExtensiblePage subpage(String name);
    public ExtensiblePage push(ExtensiblePage page);
    public void pop(ExtensiblePage page);
}
