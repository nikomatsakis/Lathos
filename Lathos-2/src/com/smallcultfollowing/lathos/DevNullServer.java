package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;
import java.util.Collections;

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
    public void renderObject(Output out, Link link, Object obj)
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
    public void renderURL(String url, Writer writer) throws IOException
    {
    }

    @Override
    public RootPage indexPage()
    {
        return DevNullPage.instance;
    }

    @Override
    public void renderObjectAsPage(Output out, Link link, Object obj)
            throws IOException
    {
    }

    @Override
    public Iterable<RootPage> rootPages()
    {
        return Collections.emptyList();
    }

    @Override
    public void join()
    {
    }

    @Override
    public void stop() throws Exception
    {
    }

}
