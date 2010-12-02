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
    public Page asPage(LathosServer server, Object obj) {
        if (obj instanceof Page) return (Page)obj;
        return null;
    }
}
