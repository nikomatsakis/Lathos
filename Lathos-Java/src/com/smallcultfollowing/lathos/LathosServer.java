package com.smallcultfollowing.lathos;


public interface LathosServer {
	
	/** Returns the "index page" for the server
	 *  (i.e., the default page to which users are
	 *  directed). */
	public Page getIndexPage();

	/** Registers a page with the server.  This is
	 *  generally not needed as pages are automatically
	 *  registered when they are linked to or embedded,
	 *  but for a top-level page it can be needed. */
	public void registerPage(Page page);
	
	/** Returns a fresh context whose stack consists only
	 *  of the "main" or index page. */
	public Context context();
	
	/** Adds a custom data handler that may be used
	 *  by {@link #addToLine(Line, Object)}. */
	public void addDataRenderer(DataRenderer dr);

	/** Adds the content of {@code o} to the
	 *  line {@code line}.  The precise means
	 *  by which this is done depends on the
	 *  type of {@code o}: strings, numbers, or
	 *  objects which implement {@link CustomOutput}
	 *  are pre-defined.  Other kinds of objects may
	 *  be handled by {@link DataRenderer} objects
	 *  which have been registered via 
	 *  {@link #addDataRenderer(DataRenderer)},
	 *  or which are pre-defined by the server.
	 *  As a last resort, {@code o.toString()}
	 *  will be used. */
	public void addToLine(Line line, Object o);
	
	/** 
	 * Blocks the current thread until the user has
	 * indicated that the Lathos session should terminate.
	 * 
	 * @throws InterruptedException
	 */
	public void join() throws InterruptedException;
	
	/**
	 * Stops the server.
	 * 
	 * @throws Exception
	 */
	public void stop() throws Exception;
	
}
