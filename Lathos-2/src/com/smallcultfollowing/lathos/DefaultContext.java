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
    public ExtensiblePage subpage(String name)
    {
        ExtensiblePage top = stack.peek();
        if(top instanceof DevNullPage) {
            return top;
        } else {
            ExtensiblePage subpage = new LogPage(name);
            top.addSubPage(name, subpage);
            return subpage;
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

}
