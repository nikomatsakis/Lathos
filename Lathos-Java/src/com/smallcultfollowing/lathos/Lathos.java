package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.lang.reflect.Field;

/** Utility methods for {@link Page} implementations */
public class Lathos {

	private static ThreadLocal<Context> context = new ThreadLocal<Context>();

	/** 
	 * Current context for this thread.  
	 * Returns {@code null} if not set. */
	public static Context context() {
		return context.get();
	}

	/** 
	 * Current context for this thread, or 
	 * {@link NoneContext#Context} if not yet set. */  
	public static Context contextOrNone() {
		Context ctx = context();
		if(ctx == null)
			return NoneContext.Context;
		return ctx;
	}

	/** 
	 * Returns a fresh context writing to page.  If there is no
	 * current context for the thread, returns 
	 * {@link NoneContext#Context}. */
	public static Context context(Page page) {
		LathosServer server = contextOrNone().server();
		Context ctx = server.context();
		ctx.push(page);
		return ctx;
	}

	/** 
	 * Sets the current context for this thread,
	 * returning the old value. 
	 * 
	 * Suggested usage:
	 * <pre>
	 *   Context oldContext = Lathos.setContext(newContext);
	 *   try {
	 *     ...
	 *   } finally {
	 *     Lathos.setContext(oldContext);
	 *   }
	 * </pre>
	 */
	public static Context setContext(Context ctx) {
		Context oldValue = context.get();
		context.set(ctx);
		return oldValue;
	}
	
	/** 
	 * Equivalent to invoking {@link #newPage(LathosServer, Page, String, Object...)}
	 * with a parent of {@code null}.
	 */
	public static Page topLevelPage(LathosServer server, String id, Object... title) {
		return newPage(server, null, id, title);
	}

	private static void initializePage(
			LathosServer server, 
			String id,
			Page page, 
			Object... title) 
	{
		Title line = new Title();
		page.addContent(line);
		for(Object o : title)
			server.addToLine(line, o);

		// No need to register the page unless the user 
		// provided an ID, otherwise they wouldn't know what
		// to type into the web browser anyhow:
		if(id != null)
			server.registerPage(page);
	}

	/** 
	 * Creates a new page, optionally specifying its parent page.
	 * 
	 * Some important details:
	 * <ul>
	 * <li> If the parent page is {@link NonePage#Page}, then this method simply
	 * returns {@link NonePage#Page}.
	 * 
	 * <li> The returned object is <b>not</b> added to {@code parent}
	 * in any way.  You probably want to either invoke {@link Page#addContent(PageContent)}
	 * to embed the returned object or else add a link to the returned object.
	 * 
	 * If you supply a specific id, however, the object will be registered with
	 * the server and thus reachable by directly specifying its URL. 
	 * </ul>
	 * 
	 * 
	 * @param server the Lathos server 
	 * @param parent the parent of the new page, if {@code null} then a top-level page is created
	 * @param id id of the new page, or {@code null} to use a default id
	 * @param title the title content for the subpage, if any
	 */
	public static Page newPage(LathosServer server, Page parent, String id, Object... title) {
		if(parent == NonePage.Page)
			return NonePage.Page;
		
		Page page = new UserPage(id, parent);
		initializePage(server, id, page, title);
		return page;	
	}
	
	/** Returns a default id for the page based on the identity hash code */
	public static String defaultId(Page page) {
		return String.format("%s[%x]", page.getClass().getSimpleName(), System.identityHashCode(page));
	}

	/** Renders a page inline using its toString value as the text for a link */
	public static void renderInLine(Page page, Output output) 
	throws IOException {
		output.startLink(page);
		output.outputText(page.toString());
		output.endLink(page);
	}
	
	/** Creates a row with each member of {@code labels} written in bold */
	public static void headerRow(Output output, Object... labels) 
	throws IOException {
		output.startRow();
		for(Object label : labels) {
			output.startColumn();
			output.startBold();
			output.outputObject(label);
			output.endBold();
			output.endColumn();
		}
		output.endRow();
	}
	
	/** Creates a row with columns for each member of {@code columns} */
	public static void row(Output output, Object... columns)
	throws IOException {
		output.startRow();

		for(Object column : columns) {
			output.startColumn();
			output.outputObject(column);
			output.endColumn();
		}
		
		output.endRow();
	}
	
	/** 
	 * Creates a default page for {@code page} that
	 * uses reflection to print out the values of all fields,
	 * except for those annotated with {@link Ignore}. */
	public static void reflectivePage(Page page, Output output)
	throws IOException
	{
		output.startPage(page);
		reflectivePageContents(page, output);
		output.endPage(page);
	}

	/** 
	 * Same as {@link #reflectivePage(Page, Output)}, but
	 * does not start or end the page, allowing the caller to
	 * add more content afterwards. */
	public static void reflectivePageContents(Page page, Output output)
	throws IOException 
	{
		output.startPar();
		output.startBold();
		output.outputText(page.toString());
		output.endBold();
		output.endPar();
		
		reflectiveFields(page, output);
		
		for(Class<?> cls = page.getClass(); cls != null; cls = cls.getSuperclass()) {
			for(Field fld : cls.getDeclaredFields()) {
				if(fld.getAnnotation(PageSubcontent.class) != null) {
					fld.setAccessible(true);
					
					try {
						Iterable<?> iter = (Iterable<?>) fld.get(page);
						if(iter != null) {
							for(Object obj : iter) {
								PageContent pc = (PageContent) obj;
								pc.renderInPage(output);
							}
						}
					} catch (ClassCastException exc) {
						outputError(output, fld, exc);
					} catch (IllegalArgumentException e) {
						outputError(output, fld, e);
					} catch (IllegalAccessException e) {
						outputError(output, fld, e);
					}
				}
			}
		}
	}

	private static void outputError(Output output, Field fld, Throwable exc)
			throws IOException {
		output.startPar();
		output.startBold();
		output.outputText("Error processing PageSubcontent field ");
		output.outputText(fld.getName());
		output.outputText(": ");
		output.outputObject(exc);
		output.endBold();
		output.endPar();
	}

	/** 
	 * Outputs a table containing the names and values of the fields of
	 * {@code page} except for those annotated with {@link Ignore}
	 * or {@link PageSubcontent}.
	 * 
	 * @see #reflectivePage(Page, Output) */
	public static void reflectiveFields(Object page, Output output)
			throws IOException 
	{
		output.startTable();
		
		Lathos.headerRow(output, "Field", "Value");
		
		for(Class<?> cls = page.getClass(); cls != null; cls = cls.getSuperclass()) {
			for(Field fld : cls.getDeclaredFields()) {
				if(fld.getAnnotation(Ignore.class) != null)
					continue;
				
				fld.setAccessible(true);
				output.startRow();
				
				output.startColumn();
				output.outputText(fld.getName());
				output.endColumn();

				try {
					output.startColumn();
					output.outputObject(fld.get(page));
					output.endColumn();
				} catch (Exception e) {
					output.outputText("Failed to access: ");
					output.outputObject(e);
				}
				
				output.endRow();
			}
		}
		output.endTable();
	}

	/** 
	 * Creates a paragraph with the given contents and adds it to {@code addToPage}.
	 * After the paragraph is created you can append additional content by 
	 * invoking {@link LathosServer#addToLine(Line, Object)} or
	 * the methods of {@link Line}.
	 *  
	 * @param server to use for rendering contents
	 * @param addToPage page to add paragraph to (or null)
	 * @param contents objects to add to the line */
	public static Para para(LathosServer server, Page addToPage, Object... contents) {
		Para para = new Para();
		
		for(Object o : contents)
			server.addToLine(para, o);
		
		if(addToPage != null)
			addToPage.addContent(para);

		return para;
	}

}
