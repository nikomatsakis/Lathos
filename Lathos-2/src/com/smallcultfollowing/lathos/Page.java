package com.smallcultfollowing.lathos;

import java.io.IOException;

/**
 * Marker interface used by the {@link PageRenderer} (installed by the default
 * setup server routine) to denote pages that can render themselves.
 * 
 * @see PageRenderer
 * @see Lathos#setupServer(LathosServer)
 */
public interface Page
{
    /** @see ObjectRenderer#renderObjectSummary(Object, Output, Link) */
    public void renderSummary(Output out, Link link) throws IOException;

    /**
     * Renders the contents of this page. The standard page header and footer
     * will already have been emitted.
     * 
     * @see Lathos#reflectiveRenderDetails(Page, Output, Link)
     */
    public void renderDetails(Output out, Link link) throws IOException;

    /**
     * Dereferences a relative link to yield the next step. For example, if
     * there is a link "/a/b/c", and "/a/b" leads to {@code this}, then this
     * method would be invoked with "c" to yield the next step. If the link were
     * "/a/b/c/d" instead, then the result of this method would then itself be
     * casted to {@link Page} and have {@link #derefPage(LathosServer, String)} invoked with
     * "d" as argument.
     * @param server TODO
     * 
     * @throws InvalidDeref
     *             if link cannot be dereferenced to an Object
     *             
     * @see Lathos#reflectiveDerefPage(Page, String)
     */
    public Object derefPage(LathosServer server, String link) throws InvalidDeref;
}
