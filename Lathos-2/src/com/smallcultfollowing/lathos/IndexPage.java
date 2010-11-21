package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Map;

/**
 * This is the class used as the default "index page" for a server.
 * If accessed, it displays a list of all installed root pages.
 * It is extensible, so users can add their own log messages. 
 */
public class IndexPage
    extends LogPage
{
    public IndexPage()
    {
        super("index");
    }

    @Override
    public synchronized void renderDetails(Output out, Link link) throws IOException
    {
        out.h1().text("Index")._h1();
        
        out.h2().text("Root Pages")._h2();
        out.ul();
        for(Map.Entry<String, Object> entry : out.server.rootPages().entrySet())
        {
            out.li();
            out.obj(new BaseLink(entry.getKey()), entry.getValue());
            out._li();
        }
        out._ul();
        
        out.h2().text("Log Messages")._h2();
        super.renderDetails(out, link);
    }
}
