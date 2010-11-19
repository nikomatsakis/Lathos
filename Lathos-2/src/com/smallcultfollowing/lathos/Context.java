package com.smallcultfollowing.lathos;

/**
 * Thread-local context for generating logs.  If logs are disabled,
 * these methods do nothing, returning null or dummy values. 
 * 
 * Generally passed via calls to {@link Lathos#context()}
 * or {@link Lathos#setContext(Context)}. 
 */
public interface Context
{
    /** Server with which this context is associated */
    LathosServer server();
    
    /**
     * Creates a new line with the given objects and adds it to the current
     * LogPage.
     */
    Line log(Object... objs);

    /**
     * Returns an object suitable for passing to {@code log} that
     * will display the given text but, when clicked, go to 
     * {@code linkTo}.
     */
    Object linked(Object linkTo, Object... text);
    
    /** Adds the given page to the current LogPage */
    void embed(Page page);

    /**
     * Adds the given page to the current LogPage, using the link name
     * {@code link}
     */
    void embed(String link, Page page);

    /**
     * Creates and returns a new page with the given name. The new page is not
     * pushed onto the stack nor is it added to the current page.
     * 
     * Note: if logs are disabled, will return {@link DevNullPage#instance}.
     * 
     * @param name
     *            a name for the new log page, or null to use a default name
     *            
     * @see #push(ExtensiblePage)
     * @see #embed(Page)
     */
    public ExtensiblePage newPage(String name);

    /** Pushes the given page at the top of the stack */
    public ExtensiblePage push(ExtensiblePage page);

    /**
     * Pops the top of the stack
     * 
     * @param page
     *            the page that should be on top of the stack, or null
     */
    public void pop(ExtensiblePage page);
}
