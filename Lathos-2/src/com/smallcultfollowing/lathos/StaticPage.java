package com.smallcultfollowing.lathos;

import java.io.IOException;

/**
 * A useful root page that is included by default.
 * Allows you to access static fields, avoiding
 * the need to register all pages.
 */
public class StaticPage
    implements RootPage
{

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.h1().text("Static")._h1();
        out.text("You can examine static fields by using a URL like ");
        out.code().text("http://localhost/static/fully.qualified.class.name/fieldName")._code();
    }

    @Override
    public Object derefPage(LathosServer server, String link)
    {
        try {
            Class<?> cls = Class.forName(link);
            return new ClassObjectPage(cls);
        } catch (ClassNotFoundException e) {
            return null;
        }
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        out.a(link).text("static");
        out._a(link);
    }

    @Override
    public String rootPageName()
    {
        return "static";
    }

}
