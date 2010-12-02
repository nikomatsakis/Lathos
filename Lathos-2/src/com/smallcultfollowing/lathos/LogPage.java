package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class LogPage
    implements ExtensiblePage, RootPage, Page.Detailed
{
    private final String name; // warning: may be null
    private final Page title;
    private final Map<String, Page> subpages = new LinkedHashMap<String, Page>();
    
    public LogPage(String name, Page title)
    {
        super();
        this.name = name;
        this.title = title;
    }

    @Override
    public void renderObjectTitle(Output out, Link link) throws IOException {
        if(title != null) {
            out.embed(link, "title", title);
        } else {
            out.text(name);
        }
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        for(Map.Entry<String, Page> entry : subpages.entrySet()) {
            RelativeLink pageLink = new RelativeLink(link, entry.getKey());
            //out.subpage();
            //out.obj(pageLink, entry.getValue());
            out.embed(pageLink, entry.getValue());
            //out._subpage();
        }
    }

    @Override
    public Object derefPage(LathosServer server, String link)
    {
        if(link.equals("title")) return title;
        return subpages.get(link);
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        if(title != null) {
            out.a(link);
            out.obj(null, title);
            out._a(link);
        } else {
            out.a(link);
            out.text(rootPageName());
            out._a(link);
        }
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
