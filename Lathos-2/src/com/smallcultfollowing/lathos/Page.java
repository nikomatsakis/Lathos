package com.smallcultfollowing.lathos;

import java.io.IOException;

public interface Page
{
    /**
     * Renders a link to this page onto {@code out}.  The link
     * {@code link}, if non-null, leads to this object.
     * 
     * Typically looks like:
     * <pre>
     * out.a(link);
     * out.text(...);
     * out._a(link);
     * </pre>
     * 
     * @see Lathos#reflectiveRenderAsLine(Object, Output, Link)
     */
    public void renderAsLine(Output out, Link link) throws IOException;

    /**
     * Renders the contents of this page. The standard page header and footer
     * will already have been emitted.
     * 
     * @see Lathos#reflectiveRenderAsPage(Page, Output, Link)
     */
    public void renderAsPage(Output out, Link link) throws IOException;

    /**
     * Dereferences a relative link to yield the next step. For example, if
     * there is a link "/a/b/c", and "/a/b" leads to {@code this}, then this
     * method would be invoked with "c" to yield the next step. If the link were
     * "/a/b/c/d" instead, then the result of this method would then itself be
     * casted to {@link Page} and have {@link #derefPage(String)} invoked with
     * "d" as argument.
     * 
     * @see Lathos#reflectiveDerefPage(Page, String)
     */
    public Object derefPage(String link);
}
