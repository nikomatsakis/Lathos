package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.Writer;
import java.util.Collection;

public interface LathosServer
{
    /** Returns the default page for the server. */
    public RootPage indexPage();

    /**
     * Sets the object which generates default links if none is provided.
     * 
     * {@code linkCache} will also be added as a root page, so that it can
     * dereference the links it generates.
     * 
     * If no link cache is set, then null or broken links will simply be
     * ignored.
     */
    public void setLinkCache(LinkCache linkCache);

    /**
     * Creates a default link that (may) lead to object using the link cache.
     * 
     * @return result from link cache, or null if no link cache installed
     * 
     * @see #setLinkCache(LinkCache)
     */
    public Link defaultLink(Object obj);

    public Iterable<RootPage> rootPages();

    /**
     * Used internally: when a log message is created, every object is run
     * through a set of configurable substitution filters that can change it to
     * another object. For example, they might invoke {@link Object#toString()}
     * and just log the string representation and not the object itself.
     * 
     * Note that substitutions occur at the time of the log statement and
     * therefore add overhead during execution.
     * 
     * @see #addSubstitutionFilter(ObjectSubst)
     */
    public Object substitute(Object obj);

    /**
     * Adds a substitution filter which will be used for all subsequent logs.
     * 
     * @param subst
     *            the subst filter
     * @see #substitute(Object)
     * @see #addRenderer(ObjectRenderer)
     */
    public void addSubstitutionFilter(ObjectSubst subst);

    /**
     * Renders {@code obj} using the installed object renderers. 
     * 
     * @throws IOException
     *             if an error occurs writing to {@code out}
     *             
     * @see ObjectRenderer#renderObjectSummary(Object, Output, Link)
     * @see #addRenderer(ObjectRenderer)
     */
    public void renderObjectSummary(Output out, Link link, Object obj) throws IOException;

    /**
     * Renders {@code obj} as a page using the installed object renderers.
     * 
     * @throws IOException
     *             if an error occurs writing to {@code out}
     *             
     * @see ObjectRenderer#renderObjectDetails(Object, Output, Link)
     * @see #addRenderer(ObjectRenderer)
     */
    public void renderObjectDetails(Output out, Link link, Object obj) throws IOException;

    /**
     * Dereferences {@code page} using the installed object renderers.
     * 
     * @see #addRenderer(ObjectRenderer)
     * @see ObjectRenderer#derefPage(Object, String)
     */
    public Object derefPage(Object page, String link);

    /**
     * Adds an object renderer. When we render a webpage, any objects in the
     * logs are first run through the installed ObjectRenderers in the order
     * they were installed. If any of them are able to render the object, we use
     * it in preference to the default rendering strategy.
     * 
     * @param render
     *            the renderer
     * @see #renderObjectSummary(Output, Link, Object)
     * @see #addSubstitutionFilter(ObjectSubst)
     */
    public void addRenderer(ObjectRenderer render);

    /**
     * Adds {@code page} as a root page. Root pages are pages whose links can
     * form the initial part of a URL.
     */
    public void addRootPage(RootPage page);

    /** @see #addRootPage(RootPage) */
    public void addRootPages(Collection<RootPage> pages);

    /** Removes {@code page} as a root page. */
    public void removeRootPage(RootPage page);

    /**
     * Returns a fresh context whose stack consists only of the "main" or index
     * page.
     */
    public Context context();

    /**
     * Process the URL {@code url} and writes the output to {@code writer}.
     */
    public void renderURL(String url, Writer writer) throws IOException;

    /** Waits until the server stops. */
    public void join();

    /** Stops the server. */
    public void stop() throws Exception;

}
