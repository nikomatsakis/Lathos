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
        super("index", null);
    }

    @Override
    public void renderObjectTitle(Output out, Link link) throws IOException {
        out.h1().text("Index")._h1();
    }

    @Override
    public synchronized void renderDetails(Output out, Link link) throws IOException
    {
        out.subpage(null, "Root Pages");
        out.ul();
        for(Map.Entry<String, Object> entry : out.server.rootPages().entrySet())
        {
            out.li();
            out.obj(new BaseLink(entry.getKey()), entry.getValue());
            out._li();
        }
        out._ul();
        out._subpage();

        out.subpage(null, "Log Messages");
        super.renderDetails(out, link);
        out._subpage();;
    }
}
