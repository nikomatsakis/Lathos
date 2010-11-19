package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class LogPage
    implements ExtensiblePage, RootPage
{
    private final String name; // warning: may be null
    private final Map<String, Page> subpages = new LinkedHashMap<String, Page>();
    
    public LogPage(String name)
    {
        super();
        this.name = name;
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.ul();
        for(Map.Entry<String, Page> entry : subpages.entrySet()) {
            RelativeLink pageLink = new RelativeLink(link, entry.getKey());
            out.li();
            out.obj(pageLink, entry.getValue());
            out._li();
        }
        out._ul();
    }

    @Override
    public Object derefPage(LathosServer server, String link)
    {
        return subpages.get(link);
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        out.a(link);
        out.text(rootPageName());
        out._a(link);
    }

    @Override
    public synchronized void addSubPage(String link, Page page)
    {
        if (link == null) {
            link = "page" + subpages.size();
        }
        subpages.put(link, page);
    }

    @Override
    public String rootPageName()
    {
        if(name == null)
            return String.format("<LogPage:%02x>", System.identityHashCode(this) % 64);

        return name;
    }

}
