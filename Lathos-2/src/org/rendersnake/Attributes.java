package org.rendersnake;

import org.apache.commons.lang.StringEscapeUtils;
import org.rendersnake.ext.jquery.ToJavascript;

/**
 * Attributes captures a set of attribute key and values.
 * 
 * @author ernestmicklei
 */
public class Attributes {
	final StringBuilder out = new StringBuilder(16);

	public Attributes() {}

	/**
	 * Create a new Attributes and add a single key,value pair. The value will be XML-escaped.
	 * @param key String (not-null)
	 * @param value String | null
	 * @return an Attributes
	 */
	public Attributes(String key, String value) {
		this.add(key, value, true);
	}
	/**
	 * Add a key=value pair to the receiver. Do not XML escape the value.
	 * 
	 * @param key
	 *            String (not-null)
	 * @param value
	 *            String | null
	 * @return the receiver, an Attributes
	 */
	public Attributes add(String key, String value) {
		if (value == null)
			return this;
		out.append(' ');
		out.append(key);
		out.append("=\"");
		out.append(value);
		out.append('\"');
		return this;
	}
	/**
	 * Add a key=value pair to the receiver. Xml escape the value if needed.
	 * 
	 * @param key
	 *            String (not-null)
	 * @param value
	 *            String | null
	 * @param doEscape
	 *            boolean
	 * @return the receiver, an Attributes
	 */
	public Attributes add(String key, String value, boolean doEscape) {
		if (value == null)
			return this;
		out.append(' ');
		out.append(key);
		out.append("=\"");
		if (doEscape) {
			out.append(StringEscapeUtils.escapeXml(value));
		} else {
			out.append(value);
		}
		out.append('\"');
		return this;
	}

	@Override
	public String toString() {
		return super.toString() + "[" + out.toString() + "]";
	}

	/**
	 * Return the key value pair(s) String ready to insert into HTML
	 * 
	 * <p>Use {@link toHtml} instead.
	 * 
	 * @return String
	 */
	@Deprecated
	public String html() {
		return this.toHtml();
	}
	/**
	 * Return the key value pair(s) String that is ready to insert into HTML.
	 * 
	 * @return String
	 */
	public String toHtml() {
		return out.toString();
	}
	
	// /////////////////////////////////////////////////////////////////////////
	//
	// Methods below are for convenience.
	//
	// /////////////////////////////////////////////////////////////////////////

    /**
     * use class_(...)
     */
    @Deprecated	
	public Attributes clazz(String cssClass) {
		return this.add("class", cssClass, false);
	}
    public Attributes class_(String cssClass) {
        return this.add("class", cssClass, false);
    }
	public Attributes onclick(ToJavascript object) {
		return this.add("onclick", object.toJavascript(), false);
	}

	public Attributes xmlLang(String xmlLang) {
		return this.add("xml:lang", xmlLang, false);
	}	
	
	// /////////////////////////////////////////////////////////////////////////
	//
	// Methods below are generated using XSLT (see /html-codegen). DO NOT EDIT
	//
	// /////////////////////////////////////////////////////////////////////////

	/**
	 * @param id
	 *            String | null
	 * @return a new Attributes
	 */
	public Attributes id(String id) {
		return new Attributes().add("id", id, true);
	}

	public Attributes lang(String lang) {
		return this.add("lang", lang, false);
	}

	public Attributes onLoad(String onLoad) {
		return this.add("onload", onLoad, false);
	}

	public Attributes onUnload(String onUnload) {
		return this.add("onunload", onUnload, false);
	}

	public Attributes onBlur(String onBlur) {
		return this.add("onblur", onBlur, false);
	}

	public Attributes onAbort(String onAbort) {
		return this.add("onabort", onAbort, false);
	}

	public Attributes onChange(String onChange) {
		return this.add("onChange", onChange, false);
	}

	public Attributes onFocus(String onFocus) {
		return this.add("onFocus", onFocus, false);
	}

	public Attributes onReset(String onReset) {
		return this.add("onReset", onReset, false);
	}

	public Attributes onSelect(String onSelect) {
		return this.add("onSelect", onSelect, false);
	}

	public Attributes onSubmit(String onSubmit) {
		return this.add("onSubmit", onSubmit, false);
	}

	public Attributes method(String method) {
		return this.add("method", method, false);
	}

	public Attributes action(String action) {
		return this.add("action", action, false);
	}

	public Attributes type(String type) {
		return this.add("type", type, false);
	}

	public Attributes name(String name) {
		return this.add("name", name, false);
	}

	/**
	 * Specifies the text direction for the content in an element
	 * 
	 * @param dir
	 *            ltr, rtl
	 * @return the receiver, an Attributes
	 */
	public Attributes dir(String dir) {
		return this.add("dir", dir, false);
	}

	//
	// Predefined HTML attributes
	// http://www.w3schools.com/tags/ref_eventattributes.asp
	//
	/**
	 * Write style="..."
	 */
	public Attributes style(String style) {
		return this.add("style", style, false);
	}

	public Attributes title(String title) {
		return this.add("title", title, false);
	}

	public Attributes accessKey(String accessKey) {
		return this.add("accesskey", accessKey, false);
	}

	public Attributes href(String href) {
		return this.add("href", href, false);
	}

	public Attributes src(String src) {
		return this.add("src", src, false);
	}
    public Attributes media(String src) {
        return this.add("media", src, false);
    }
    public Attributes content(String src) {
        return this.add("content", src, false);
    }
    public Attributes for_(String src) {
        return this.add("for", src, false);
    }
    public Attributes align(String src) {
        return this.add("align", src, false);
    }   
    public Attributes width(String src) {
        return this.add("width", src, false);
    }  
    public Attributes height(String src) {
        return this.add("height", src, false);
    }     
    public Attributes alt(String src) {
        return this.add("alt", src, false);
    }
}
