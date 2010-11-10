package org.rendersnake;

import org.rendersnake.ext.jquery.JQueryCanvas;

public abstract class AttributesFactory {
    /**
     * Create a new JQueryCanvas to compose expressions.
     * @param selector , A string containing a selector expression
     * @see {@link http://api.jquery.com/jQuery/}
     * @return a new JQueryCanvas
     */
    public static JQueryCanvas $(String selector) {
        return new JQueryCanvas().jQuery(selector);
    }
    /**
     * Create a new JQueryCanvas to compose expressions.
     * @param selector , A string containing a selector expression
     * @param context , A DOM Element, Document, or jQuery to use as context
     * @see {@link http://api.jquery.com/jQuery/}
     * @return a new JQueryCanvas
     */
    public static JQueryCanvas $(String selector, String context) {
        return new JQueryCanvas().jQuery(selector,context);
    }        
        
    ///////////////////////////////////////////////////////////////////////////
    //
    //  Methods below are generated using XSLT (see /html-codegen). DO NOT EDIT
    //   
    ///////////////////////////////////////////////////////////////////////////
    
    public static Attributes href(String href) {
        return new Attributes().add("href", href, false);
    }
    public static Attributes id(String id) {
        return new Attributes().add("id", id, false);
    }
    public static Attributes name(String name) {
        return new Attributes().add("name", name, false);
    }
    public static Attributes onclick(String javascript){
        return new Attributes().add("onclick", javascript, false);
    }
    // TODO: needs to be generated
    public static Attributes onclick(JQueryCanvas jq) {
       return onclick(jq.toJavascript());
    }
    /**
     * use class_(...)
     */
    @Deprecated
    public static Attributes clazz(String cssClass) {
        return new Attributes().add("class", cssClass, false);
    }  
    public static Attributes class_(String cssClass) {
        return new Attributes().add("class", cssClass, false);
    }    
    public static Attributes http_equiv(String equiv) {
        return new Attributes().add("http-equiv", equiv, false);
    }  
    public static Attributes rel(String rel) {
        return new Attributes().add("rel", rel, false);
    }
    public static Attributes for_(String src) {
        return new Attributes().add("for", src, false);
    }   
    public static Attributes type(String src) {
        return new Attributes().add("type", src, false);
    } 
    public static Attributes src(String src) {
        return new Attributes().add("src", src, false);
    }    
}
