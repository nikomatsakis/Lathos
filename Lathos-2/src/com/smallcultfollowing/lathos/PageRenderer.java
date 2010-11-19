package com.smallcultfollowing.lathos;

import java.io.IOException;

/**
 * An object renderer that looks for instances of {@link Page} and invokes their
 * methods.  Installed as part of the default setup.
 * 
 * @see Page
 */
public class PageRenderer
    implements ObjectRenderer
{

    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link) throws IOException
    {
        if (obj instanceof Page) {
            ((Page) obj).renderSummary(out, link);
            return true;
        }
        return false;
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link) throws IOException
    {
        if (obj instanceof Page) {
            ((Page) obj).renderDetails(out, link);
            return true;
        }
        return false;
    }

    @Override
    public Object derefPage(Object obj, LathosServer server, String link) throws InvalidDeref
    {
        if (obj instanceof Page) {
            return ((Page) obj).derefPage(server, link);
        }
        throw InvalidDeref.instance;
    }

}
