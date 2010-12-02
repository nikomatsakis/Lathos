package com.smallcultfollowing.lathos;

import java.io.IOException;

/** Customizes how an object is rendered to HTML and how it behaves in URLs */
public interface ObjectRenderer
{
    /**
     * Attempts to create a page representing {@code obj} so that it
     * can be rendered.  Note that you may wish to return a subtype of
     * {@link Page} such as {@link Page.Detailed}.
     *
     * @param server lathos server on which
     * @param obj the object being converted
     * @return null if obj cannot be converted to a page
     */
    public Page asPage(LathosServer server, Object obj);
}
