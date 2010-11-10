package com.smallcultfollowing.lathos;

import static org.rendersnake.AttributesFactory.href;

import java.io.IOException;
import java.util.LinkedHashMap;
import java.util.Map;

public class LogPage
    implements ExtensiblePage
{
    protected final String name;
    private final Map<String, Page> subpages = new LinkedHashMap<String, Page>();
    
    public LogPage(String name)
    {
        super();
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
        out.a(href(Lathos.toString(link)));
        out.text(name);
        out._a();
    }

    @Override
    public synchronized void addSubPage(String id, Page page)
    {
        if (id == null) {
            id = "page" + subpages.size();
        }
        subpages.put(id, page);
    }

}
