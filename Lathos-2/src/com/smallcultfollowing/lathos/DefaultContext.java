package com.smallcultfollowing.lathos;

import java.util.Stack;

public class DefaultContext
    implements Context
{
    private final LathosServer server;
    private final Stack<ExtensiblePage> stack;

    public DefaultContext(LathosServer server)
    {
        super();
        this.server = server;
        this.stack = new Stack<ExtensiblePage>();
    }

    private Object[] substitute(Object[] objs)
    {
        for (int i = 0; i < objs.length; i++) {
            objs[i] = server.substitute(objs[i]);
        }
        return objs;
    }

    @Override
    public Line log(Object... objs)
    {
        if (!stack.isEmpty()) {
            ExtensiblePage top = stack.peek();
            if (top instanceof DevNullPage) {
                return DevNullLine.instance;
            } else {
                ArrayLine line = new ArrayLine(substitute(objs));
                top.addSubPage(null, line);
                return line;
            }
        } else
            return DevNullLine.instance;
    }

    @Override
    public ExtensiblePage newPage(String name, Object... title)
    {
        if (!stack.isEmpty()) {
            ExtensiblePage top = stack.peek();
            if (top instanceof DevNullPage) {
                return top;
            } else {
                return new LogPage(name, new ArrayLine(substitute(title)));
            }
        } else
            return DevNullPage.instance;
    }

    @Override
    public ExtensiblePage push(ExtensiblePage page)
    {
        stack.push(page);
        return page;
    }

    @Override
    public void pop(ExtensiblePage page)
    {
        ExtensiblePage popped = stack.pop();
        if (page != null && page != popped)
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
        if (!stack.isEmpty()) {
            stack.peek().addSubPage(link, page);
        }
    }

    @Override
    public Page linked(Object linkTo, Object... text)
    {
        return new Linked(linkTo, new ArrayLine(substitute(text)));
    }

    @Override
    public Page i18n(String fmt, Object... args)
    {
        return new I18nMessage(fmt, substitute(args));
    }

    @Override
    public void append(Line line, Object... objs)
    {
        line.addObjectsToLine(substitute(objs));
    }

    @Override
    public LathosServer server()
    {
        return server;
    }

    @Override
    public ExtensiblePage topPage()
    {
        if (stack.isEmpty())
            return DevNullPage.instance;
        else
            return stack.peek();
    }

}
