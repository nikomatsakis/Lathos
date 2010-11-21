package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Collection;
import java.util.Map;
import java.util.ResourceBundle;

/**
 * A {@link LathosServer} is what processes requests to display pages.
 * Basically, it's a web server. It is also the main point of configuration for
 * matters such as localization, controlling how objects are rendered, etc.
 * 
 * A note on synchronization: like all Lathos objects, all public methods on
 * LathosServers must be generally thread-safe. In addition, when a LathosServer
 * is rendering a page, it will be locked, which means that if you have an
 * {@link ObjectRenderer} or {@link Page} and its rendering methods are invoked,
 * the server which it receives as arguments is locked and so will not be
 * concurrently modified.
 */
public interface LathosServer
{
    public static final String indexName = "index";

    /**
     * Returns the resource bundle to use for translating strings in this
     * server.
     * 
     * @return the resource bundle set, or null if none has been set
     * @see #setResourceBundle(ResourceBundle)
     * @see ResourceBundle#getBundle(String)
     */
    public ResourceBundle getResourceBundle();

    /**
     * Returns the resource bundle to use for translating strings in this
     * server.
     */
    public void setResourceBundle(ResourceBundle bundle);

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
     * @see ObjectRenderer#derefPage(Object, LathosServer, String)
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
     * Adds {@code page} as a root page using {@link RootPage#rootPageName()} as
     * its link.
     */
    public void addRootPage(RootPage page);

    /** @see #addRootPage(RootPage) */
    public void addRootPages(Collection<RootPage> pages);

    /**
     * Adds a "root page", meaning one that is accessible with a url like
     * {@code /foo}.
     * 
     * @param link
     *            the url by which the page is accessible ({@code "foo"}, in the
     *            example given above)
     * @param page
     *            the page to display
     */
    public void addRootPage(String link, Object page);

    /**
     * Returns an unmodifiable map containing the current root pages. Note that
     * concurrent calls to {@link #addRootPage(String, Object)} will modify this
     * map, so don't do that.
     */
    public Map<String, Object> rootPages();

    /** Removes {@code page} as a root page. */
    public void removeRootPage(RootPage page);

    /**
     * Returns a fresh context. The stack on the context is empty.
     */
    public Context context();

    /**
     * Returns the root page mapped to {@link #indexName}, if any.
     */
    public Object getIndexPage();

    /** Waits until the server stops. */
    public void join();

    /** Stops the server. */
    public void stop() throws Exception;

    /**
     * When we are outputting objects, we will stop embedding after a certain
     * point to prevent infinite recursion.  This allows you to configure the
     * default cut-off point.
     */
    public void setMaxEmbedDepth(int maxDepth);

    public int getMaxEmbedDepth();

    /**
     * The delegate controls matters of "look-and-feel".
     */
    public LathosServerDelegate getDelegate();

    /**
     * Sets the delegate for this server.  Must not be null.
     * 
     * @see #getDelegate()
     */
    public void setDelegate(LathosServerDelegate delegate);

}
