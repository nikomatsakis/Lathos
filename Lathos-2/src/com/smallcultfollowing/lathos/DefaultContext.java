package com.smallcultfollowing.lathos;

import java.util.Stack;

public class DefaultContext
    implements Context
{
    private final LathosServer server;
    private final Stack<ExtensiblePage> stack;
    
    public DefaultContext(LathosServer server, ExtensiblePage firstPage)
    {
        super();
        this.server = server;
        this.stack = new Stack<ExtensiblePage>();
        stack.push(firstPage);
    }

    @Override
    public Line log(Object... objs)
    {
        ExtensiblePage top = stack.peek();
        if(top instanceof DevNullPage) {
            return DevNullLine.instance;
        } else {
            ArrayLine line = new ArrayLine(server, objs);
            top.addSubPage(null, line);
            return line;
        }
    }
    
    @Override 
    public ExtensiblePage newPage(String name)
    {
        ExtensiblePage top = stack.peek();
        if(top instanceof DevNullPage) {
            return top;
        } else {
            return new LogPage(name);
        }
    }
    
    @Override
    public ExtensiblePage push(ExtensiblePage page) {
        stack.push(page);
        return page;
    }
    
    @Override
    public void pop(ExtensiblePage page) {
        ExtensiblePage popped = stack.pop();
        if(page != null && page != popped) 
            throw new PoppedWrongPageException(popped, page);
    }

    @Override
    public void embed(Page page)
    {
        embed(null, page);
    }

    @Override
    public void embed(String link, Page page)
    {
        stack.peek().addSubPage(link, page);
    }

    @Override
    public Object linked(Object linkTo, Object... text)
    {
        return new Linked(linkTo, new ArrayLine(server, text));
    }

    @Override
    public LathosServer server()
    {
        return server;
    }

}
