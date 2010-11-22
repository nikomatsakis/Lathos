package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Collection;
import java.util.Collections;
import java.util.Map;
import java.util.ResourceBundle;

public class DevNullServer
    implements LathosServer
{
    public static final DevNullServer instance = new DevNullServer();

    @Override
    public Object substitute(Object obj)
    {
        return null;
    }

    @Override
    public void addSubstitutionFilter(ObjectSubst subst)
    {
    }

    @Override
    public void renderObjectSummary(Output out, Link link, Object obj)
    {
    }

    @Override
    public void addRenderer(ObjectRenderer render)
    {
    }

    @Override
    public void addRootPage(RootPage page)
    {
    }

    @Override
    public void addRootPages(Collection<RootPage> pages)
    {
    }

    @Override
    public Context context()
    {
        return DevNullContext.instance;
    }

    @Override
    public RootPage getIndexPage()
    {
        return DevNullPage.instance;
    }

    @Override
    public void renderObjectDetails(Output out, Link link, Object obj)
            throws IOException
    {
    }

    @Override
    public void join()
    {
    }

    @Override
    public void stop() throws Exception
    {
    }

    @Override
    public void setLinkCache(LinkCache linkCache)
    {
    }

    @Override
    public Link defaultLink(Object obj)
    {
        return null;
    }

    @Override
    public void removeRootPage(RootPage page)
    {
    }

    @Override
    public Object derefPage(Object page, String link)
    {
        return null;
    }

    @Override
    public ResourceBundle getResourceBundle()
    {
        return null;
    }

    @Override
    public void setResourceBundle(ResourceBundle bundle)
    {
    }

    @Override
    public void addRootPage(String link, Object page)
    {
    }

    @Override
    public Map<String, Object> rootPages()
    {
        return Collections.emptyMap();
    }

    @Override
    public void setMaxEmbedDepth(int maxDepth)
    {
    }

    @Override
    public int getMaxEmbedDepth()
    {
        return 0;
    }

    @Override
    public LathosServerDelegate getDelegate()
    {
        return null;
    }

    @Override
    public void setDelegate(LathosServerDelegate delegate)
    {
    }

    @Override
    public void openInBrowser()
    {
    }

}
