package com.smallcultfollowing.lathos;


/**
 * Interface used to instrument code.
 *   
 * Note: Unlike most interfaces, context objects
 * are <b>not thread-safe!</b>
 */
public interface Context {

	/** 
	 * Returns the server associated with this context. */
	LathosServer server();
	
	/** 
	 * Creates a fresh context with the same stack as the
	 * current context.  This can then be used on a separate 
	 * thread. */
	Context context();
	
	/** 
	 * Returns the page on the top of the stack. */
	public Page topPage();
	
	/** 
	 * Creates and pushes a new top-level-page. 
	 *   
	 * @param id the identifier of the page.  Should generally
	 * be unique from all other top-level pages.  If you pass
	 * null, a unique number will be inserted.
	 * @param title the title content of the page. 
	 * @return the top of the stack (generally a new top-level page,
	 * unless using {@link NoneContext})
	 * @see Lathos#topLevelPage(LathosServer, String, Object...) */
	public Page pushTopLevel(String id, Object... title);

	/** 
	 * Creates and pushes a new child of the current page.
	 * Adds a link to this child page in the current page. 
	 * 
	 * @param id the id of the new page.  Should generally be unique from all
	 * other children of this page.  If you pass null, a unique
	 * number will be inserted. 
	 * @param title the title content of the page.
	 * @return the top of the stack (generally a new subpage,
	 * unless using {@link NoneContext})
	 * @see Lathos#newPage(LathosServer, Page, String, Object...) */
	public Page pushLinkedChild(String id, Object... title);
	
	/**
	 * As {@link #pushLinkedChild(String, Object...)}, but
	 * embeds the child directly in the parent page.
	 */
	public Page pushEmbeddedChild(String id, Object... title);


	/** 
	 * Pushes {@code page} onto the stack 
	 * @param page page to push on the stack
	 * @return {@code page} 
	 */
	public Page push(Page page);
	
	/** 
	 * Pushes a dummy page onto the stack which disables all
	 * logging until it is popped, or a top-level page is
	 * pushed in its stead. */   
	public void pushDisabledPage();
	
	/** 
	 * Embeds top of stack into {@code page}. */
	public void embedIn(Page page);
	
	/** 
	 * Adds new line with {@code line} contents to top of stack. */
	public void log(Object... line);
	
	/** 
	 * Appends {@code line} to the last logged line of top of stack. */
	public void append(Object... line);
	
	/** 
	 * Pops top of the stack, which must equal {@code page} 
	 * (unless {@code page} is null). */
	public void pop(Page page);
	
	/** 
	 * Creates a link to {@code page} that can be used in
	 * {@link #log(Object...)}, etc. */
	public CustomOutput link(Page page, Object... content);
	
}
