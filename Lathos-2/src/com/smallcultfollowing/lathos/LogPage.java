package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class LogPage
    implements ExtensiblePage, RootPage
{
    private final String name;
    private final Map<String, Page> subpages = new LinkedHashMap<String, Page>();
    
    public LogPage(String name)
    {
        super();
        if(name == null) {
            name = String.format("<LogPage:%02x>", System.identityHashCode(this) % 64);
        }
        this.name = name;
    }

    @Override
    public void renderAsPage(Output out, Link link) throws IOException
    {
        out.ul();
        for(Map.Entry<String, Page> entry : subpages.entrySet()) {
            RelativeLink pageLink = new RelativeLink(link, entry.getKey());
            out.li();
            out.renderObject(pageLink, entry.getValue());
            out._li();
        }
        out._ul();
    }

    @Override
    public Object derefPage(String link)
    {
        return subpages.get(link);
    }

    @Override
    public void renderAsLine(Output out, Link link) throws IOException
    {
        out.a(link);
        out.text(name);
        out._a(link);
    }

    @Override
    public synchronized void addSubPage(String id, Page page)
    {
        if (id == null) {
            id = "page" + subpages.size();
        }
        subpages.put(id, page);
    }

    @Override
    public String rootPageName()
    {
        return name;
    }

}
