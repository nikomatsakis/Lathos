package com.smallcultfollowing.lathos;

import java.io.IOException;

/** Customizes how an object is rendered to HTML and how it behaves in URLs */
public interface ObjectRenderer
{
    /**
     * Renders a summary of {@code obj} onto {@code out}.  The link
     * {@code link}, if non-null and {@link Link#isValid() valid}, 
     * leads to this object.
     * 
     * Typically looks like:
     * <pre>
     * out.a(link);
     * out.text(...);
     * out._a(link);
     * </pre>
     * 
     * @see Lathos#reflectiveRenderSummary(Object, Output, Link)
     */
    public boolean renderObjectSummary(Object obj, Output out, Link link)
    throws IOException;

    /** @see #renderObjectSummary(Object, Output, Link) */
    public boolean renderObjectDetails(Object obj, Output out, Link link)
    throws IOException;
    
    /**
     * Dereferences a relative link to yield the next step. For example, if
     * there is a link "/a/b/c", and "/a/b" leads to {@code this}, then this
     * method would be invoked with "c" to yield the next step. If the link were
     * "/a/b/c/d" instead, then the result of this method would then itself be
     * casted to {@link Page} and have {@link #derefPage(String)} invoked with
     * "d" as argument.
     * @param server TODO
     * 
     * @return the result of the dereference.  
     * @throws InvalidDeref if the link cannot be resolved against obj
     */
    public Object derefPage(Object obj, LathosServer server, String link) throws InvalidDeref;
}
