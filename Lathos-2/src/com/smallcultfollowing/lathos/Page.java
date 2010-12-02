package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.EnumSet;

/**
 * Interface that allows Lathos to interface with an Object.
 * If you wish to customize how an object is displayed, implement
 * this interface and its methods.  If you do not have access to
 * the source of an object, you can add an {@link ObjectRenderer}
 * instance to the server.
 * 
 * @see ObjectRenderer
 * @see Lathos#setupServer(LathosServer)
 */
public interface Page
{
    /**
     * Renders a short summary of this object.
     * Think of it as the HTML version of {@link Object#toString()}.
     *
     * Typically looks like:
     * <pre>
     * out.a(link);
     * out.text(toString());
     * out._a(link);
     * </pre>
     *
     * @param out where to render the summary
     * @param link the link that leads to this object, may be invalid or null
     */
    public void renderSummary(Output out, Link link) throws IOException;

    /**
     * Dereferences a relative link to yield the next step. For example, if
     * there is a link "/a/b/c", and "/a/b" leads to {@code this}, then this
     * method would be invoked with "c" to yield the next step. If the link were
     * "/a/b/c/d" instead, then the result of this method would then itself be
     * casted to {@link Page} and have {@link #derefPage(LathosServer, String)} invoked with
     * "d" as argument.
     *
     * @param server The lathos server doing the dereference
     *
     * @throws InvalidDeref
     *             if link cannot be dereferenced to an Object
     */
    public Object derefPage(LathosServer server, String link) throws InvalidDeref;
    
    public interface Detailed extends Page {
        /**
         * Renders the title section of the object's page.  This method is
         * only invoked when the object is the object named by the URL, or
         * is embedded within it.
         */
        public void renderObjectTitle(Output out, Link link) throws IOException;

        /**
         * Renders the content section of the object's page. This method is
         * only invoked when the object is the object named by the URL, or
         * is embedded within it.
         */
        public void renderDetails(Output out, Link link) throws IOException;
    }
}
