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

}
