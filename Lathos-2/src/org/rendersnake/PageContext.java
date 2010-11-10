package org.rendersnake;

import java.io.IOException;

import org.rendersnake.ext.debug.Inspector;
/**
 * PageContext provides the interface to page-render scoped variables.
 * During rendering, each component can set or get values from a PageContext.
 * Values set by a component are only visible to the component and all its child components.
 * 
 * @author ernestmicklei
 *
 */
public class PageContext {
    /**
     * Storage of the values for each component nesting level
     */
    public StackedHashMap attributes = new StackedHashMap();
    /**
     * Store a value accessed by a key
     * @param key , not null
     * @param value , any object
     */
    public void set(String key, Object value) {
        attributes.put(key, value);
    }
    /**
     * Answer the value stored by a key
     * @param key , not null
     * @return Object | null
     */
    public Object get(String key) {
        return attributes.get(key);
    }
    /**
     * Answer the String value stored by a key
     * @param key , not null
     * @return Object | null
     */
    public String getString(String key) {
        return (String) attributes.get(key);
    }
    public void set(String key,int number) {
        attributes.put(key, number);
    }
    public int getInt(String key) {
        return (Integer) attributes.get(key);
    }
    public void set(String key,boolean trueOrFalse) {
        attributes.put(key, trueOrFalse);
    }
    public boolean getBoolean(String key) {
        return (Boolean) attributes.get(key);
    }
    /**
     * The HtmlCanvas is about to render a new component.
     * This means values are stored on the next nesting level.
     */
    protected void beginRender() {
        attributes.push();
    }
    /**
     * The HtmlCanvas is finished rendering a component.
     * This means values are the current nesting level are unavailable.
     */
    protected void endRender() {
        attributes.pop();
    }
    /**
     * Render inspection information using the (debug) canvas
     * @param canvas
     * @throws IOException
     */
    public void renderForInpectorOn(Inspector inspector,HtmlCanvas canvas) throws IOException {
        canvas.text("{d=" + attributes.getDepth() + "}*");
        //canvas
        //    .pre()
        //    .text(attributes.toString())
        //    ._pre();
    }
    /**
     * Render error reporting information using the (debug) canvas
     * @param canvas
     * @throws IOException
     */    
    public void renderForErrorOn(HtmlCanvas canvas) throws IOException {
        canvas.text("{d=" + attributes.getDepth() + "}*");
        canvas
            .pre()
            .text(attributes.toString())
            ._pre();
    }    
}
