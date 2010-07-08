package com.smallcultfollowing.lathos.model;


/**
 * Interface used to instrument code.
 *   
 * Note: Unlike most interfaces, context objects
 * are <b>not thread-safe!</b>
 */
public interface Context {

	/** Returns the server associated with this context. */
	LathosServer server();
	
	/** Creates a fresh context with the same stack as the
	 *  current context.  This can then be used on a separate 
	 *  thread. */
	Context context();
	
	public String freshId();
	
	/** Returns the page on the top of the stack. */
	public Page topPage();
	
	/** Creates and pushes on the stack a top-level page.
	 *   
	 *  @param id the identifier of the page.  Should generally
	 *  be unique from all other top-level pages.
	 *  
	 *  @param title the title content of the page. 
	 *  
	 *  @return the new page created */
	public Page pushTopLevel(String id, Object... title);

	/** Creates a new child of the current page.  Automatically
	 *  inserts a breadcrumb trail for all other pushed parents
	 *  in this contenxt.
	 *  
	 *  @param id the id of the new page.  Should generally be unique from all
	 *  other children of this page.  If you pass null, a unique
	 *  number will be inserted. 
	 *  
	 *  @param title the title content of the page.  
	 *  
	 *  @return the new page created */
	public Page pushChild(String id, Object... title);
	
	/** Given a page, pushes it onto the stack. */
	public void push(Page page);
	
	/** Pushes a dummy page onto the stack which disables all
	  * logging until it is popped, or a top-level page is
	  * pushed in its stead. */   
	public void pushDisabledPage();
	
	/** Embeds top of stack into {@code page}. */
	public void embedIn(Page page);
	
	/** Adds new line with {@code line} contents to top of stack. */
	public void log(Object... line);
	
	/** Appends {@code line} to the last logged line of top of stack. */
	public void append(Object... line);
	
	/** Pops top of the stack, which must equal {@code page} 
	 *  (unless {@code page} is null). */
	public void pop(Page page);
	
	/** Creates a link to {@code page} that can be used in
	 *  {@link #log(Object...)}, etc. */
	public CustomOutput link(Page page, Object... content);
	
}
