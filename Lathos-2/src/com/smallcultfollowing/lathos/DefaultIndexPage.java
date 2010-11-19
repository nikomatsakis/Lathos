package com.smallcultfollowing.lathos;

import java.io.IOException;

public class DefaultIndexPage
    extends LogPage
    implements IndexPage
{
    public DefaultIndexPage()
    {
        super("index");
    }

    @Override
    public synchronized void renderDetails(Output out, Link link) throws IOException
    {
        out.h1().text("Index")._h1();
        
        out.h2().text("Root Pages")._h2();
        out.ul();
        for(RootPage rootPage : out.server.rootPages()) {
            out.li();
            out.obj(new BaseLink(rootPage), rootPage);
            out._li();
        }
        out._ul();
        
        out.h2().text("Log Messages")._h2();
        super.renderDetails(out, link);
    }
}
