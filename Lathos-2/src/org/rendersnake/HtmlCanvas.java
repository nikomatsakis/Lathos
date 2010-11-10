package org.rendersnake;

import java.io.IOException;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Stack;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.apache.commons.lang.StringEscapeUtils;

/**
 * HtmlCanvas provide the api to write HTML elements on a io.Writer. It knows
 * about the HttpServlet request and response. It holds a PageContext for
 * passing non-query parameters to other Renderable instances.
 * 
 * @author ernestmicklei
 * 
 */
public class HtmlCanvas {

    // Required fields
    protected Writer out;
    protected Stack<String> stack = new Stack<String>();

    // Optional fields
    public HttpServletRequest request;
    public HttpServletResponse response;
    private PageContext pageContext;

    public HtmlCanvas(HttpServletRequest request, HttpServletResponse response, Writer out) {
        this.request = request;
        this.response = response;
        this.out = out;
    }

    public HtmlCanvas(Writer output) {
        super();
        this.out = output;
        stack.setSize(64); // nesting depth of tags
    }

    public HtmlCanvas() {
        super();
        this.out = new StringWriter(1024);
    }
       
    /**
     * Answer a String with tracing information about the receiver.
     * 
     * @return String
     */
    public String toString() {
        final StringBuilder buffer = new StringBuilder();
        buffer.append(super.toString());
        buffer.append("[depth=");
        buffer.append(stack.size());
        buffer.append(']');
        buffer.append(stack);
        return buffer.toString();
    }

    /**
     * Return the current contents of the output writer.
     * 
     * @return String
     */
    public String toHtml() {
        return out.toString();
    }    
    
    /**
     * Write plain text after HTML escaping it. No need to close().
     * 
     * @param unescapedString
     * @return HTMLCanvas , the receiver
     * @throws IOException
     */
    public HtmlCanvas text(String unescapedString) throws IOException {
        StringEscapeUtils.escapeHtml(out, unescapedString);
        return this;
    }

    /**
     * Write some text. If escapedNeeded then HTML escape the text before
     * writing it.
     * 
     * @param text
     *            String , HTML or plain text
     * @param escapeNeeded
     *            , boolean
     * @return HTMLCanvas , the receiver
     * @throws IOException
     */
    public HtmlCanvas text(String text, boolean escapeNeeded) throws IOException {
        if (escapeNeeded) {
            StringEscapeUtils.escapeHtml(out, text);
        } else {
            this.out.write(text);
        }
        return this;
    }

    /**
     * Close the most recent opened tag.
     * 
     * @return the receiver, a HtmlCanvas
     * @throws IOException
     */
    public HtmlCanvas close() throws IOException {
        out.write(stack.pop());
        return this;
    }

    /**
     * Close the most recent opened tag and expect it to be #expectedTag
     * 
     * @return the receiver, a HtmlCanvas
     * @throws IOException, IllegalStateException
     */
    public HtmlCanvas close(String expectedTag) throws IOException {
    	String popped = stack.pop();
        if (!popped.equals(expectedTag))
            throw new IllegalStateException("Tag nesting error; expected [" + popped + "] actual [" + expectedTag + "]");
        out.write(popped);
        return this;
    }
    public HtmlCanvas script_javascript(String url) throws IOException {
        out.write("<script src=\"");
        out.write(url);
        out.write("\" type=\"text/javascript\" />");
        return this;
    }
    public HtmlCanvas link_stylesheet(String url) throws IOException {
        out.write("<link href=\"");
        out.write(url);
        out.write("\" type=\"text/css\" rel=\"stylesheet\" />");
        return this;
    }
    /**
     * Opens the <em>style</em> tag, with the type attribute set to <em>text/css</em>.
     *
     * <p>Close this tag by calling {@link #_style()} (the end tag is required).
     *
     * <p>This method exists for programming and reading convenience.
     * 
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */    
    public HtmlCanvas style_css() throws IOException {
        out.write("<style type=\"text/css\">");
        stack.push("</style>");
        return this;
    }
    /**
     * Opens and closes the <em>p</em> tag, with its content set to the <em>text</em>.
     *
     * <p>This method exists for programming and reading convenience.
     * 
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */  
    public HtmlCanvas p_text(String text) throws IOException {
        out.write("<p>");
        out.write(text);
        out.write("</p>");
        return this;
    }
    /**
     * Render the component using the receiver. If available, prepare the
     * pageContext for accessing variables by the components.
     * 
     * @param component, a Renderable instance
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */  
    public HtmlCanvas render(Renderable component) throws IOException {
        if (pageContext != null) {
            pageContext.beginRender();
            component.renderOn(this);
            pageContext.endRender();
            return this;
        } else {
            component.renderOn(this);
        }
        return this;
    }

    /**
     * Factory method for an Attributes instance.
     * <p/>
     * This allows for subclass specialization. Also conforms to the fluent
     * interface compared to "new Attributes()"
     * 
     * @return a new Attributes
     */
    public Attributes attributes() {
        return new Attributes();
    }

    /**
     * Write the open tag &lt;{tagName}>. Requires close().
     * 
     * @param tagName
     *            String, cannot be null
     * @return the receiver, a HtmlCanvas
     * @throws IOException
     * 
     * @see #close()
     */
    public HtmlCanvas tag(String tagName) throws IOException {
        out.write('<');
        out.write(tagName);
        out.write('>');
        StringBuilder buffer = new StringBuilder(tagName.length()+3);
        buffer.append('<').append('/').append(tagName).append('>');
        stack.push(buffer.toString());
        return this;
    }

    public HtmlCanvas tag_close(String tagName) throws IOException {
        StringBuilder buffer = new StringBuilder(tagName.length()+3);
        buffer.append('<').append('/').append(tagName).append('>');
        return this.close(buffer.toString());        
    }
    
    /**
     * Write the open tag with attributes {attrs}. Requires close().
     * 
     * @param tagName
     *            String, cannot be null
     * @param attrs
     *            Attributes, cannot be null
     * @return the receiver, aHtmlCanvas
     * @throws IOException
     * 
     * @see #close()
     */
    public HtmlCanvas tag(String tagName, Attributes attrs) throws IOException {
        out.write('<');
        out.append(tagName);
        out.write(attrs.toHtml());
        out.write('>');
        stack.push("</" + tagName + '>');
        return this;
    }
    
    /**
     * Answer whether the receiver has instantiated a PageContext.
     * @return
     */
    public boolean hasPageContext() { return pageContext != null; }
    
    /**
     * Answer the pageContext. Create one if absent.
     * 
     * @return the context
     */
    public PageContext getPageContext() {
        if (null == pageContext)
            pageContext = new PageContext();
        return pageContext;
    }

    // ////////////////////////////////////////////////////////////////
    //
    // Methods below are generated using XSLT (see /html-codegen). DO NOT EDIT
    //
    // ////////////////////////////////////////////////////////////////

    /**
     * Opens the <em>a</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_a()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas a() throws IOException {
        out.write("<a>");
        stack.push("</a>");
        return this;
    }

    /**
     * Opens the <em>a</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_a()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>named link end
     * <dt>href<dd>URI for linked resource
     * <dt>hreflang<dd>language code
     * <dt>type<dd>advisory content type
     * <dt>rel<dd>forward link types
     * <dt>rev<dd>reverse link types
     * <dt>charset<dd>char encoding of linked resource
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>shape<dd>for use with client-side image maps
     * <dt>coords<dd>for use with client-side image maps
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>the element got the focus
     * <dt>onblur<dd>the element lost the focus
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>target<dd>render in this frame
     * <dt>tabindex<dd>position in tabbing order
     * <dt>accesskey<dd>accessibility key character
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas a(Attributes attrs) throws IOException {
        out.write("<a");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</a>");
        return this;
    }

	/**
     * Closes the <em>a</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _a() throws IOException {
        return this.close("</a>");
    }

    /**
     * Opens the <em>abbr</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_abbr()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas abbr() throws IOException {
        out.write("<abbr>");
        stack.push("</abbr>");
        return this;
    }

    /**
     * Opens the <em>abbr</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_abbr()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas abbr(Attributes attrs) throws IOException {
        out.write("<abbr");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</abbr>");
        return this;
    }

	/**
     * Closes the <em>abbr</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _abbr() throws IOException {
        return this.close("</abbr>");
    }

    /**
     * Opens the <em>acronym</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_acronym()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas acronym() throws IOException {
        out.write("<acronym>");
        stack.push("</acronym>");
        return this;
    }

    /**
     * Opens the <em>acronym</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_acronym()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas acronym(Attributes attrs) throws IOException {
        out.write("<acronym");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</acronym>");
        return this;
    }

	/**
     * Closes the <em>acronym</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _acronym() throws IOException {
        return this.close("</acronym>");
    }

    /**
     * Opens the <em>em</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_em()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas em() throws IOException {
        out.write("<em>");
        stack.push("</em>");
        return this;
    }

    /**
     * Opens the <em>em</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_em()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas em(Attributes attrs) throws IOException {
        out.write("<em");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</em>");
        return this;
    }

	/**
     * Closes the <em>em</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _em() throws IOException {
        return this.close("</em>");
    }

    /**
     * Opens the <em>strong</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_strong()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas strong() throws IOException {
        out.write("<strong>");
        stack.push("</strong>");
        return this;
    }

    /**
     * Opens the <em>strong</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_strong()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas strong(Attributes attrs) throws IOException {
        out.write("<strong");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</strong>");
        return this;
    }

	/**
     * Closes the <em>strong</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _strong() throws IOException {
        return this.close("</strong>");
    }

    /**
     * Opens the <em>cite</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_cite()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas cite() throws IOException {
        out.write("<cite>");
        stack.push("</cite>");
        return this;
    }

    /**
     * Opens the <em>cite</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_cite()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas cite(Attributes attrs) throws IOException {
        out.write("<cite");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</cite>");
        return this;
    }

	/**
     * Closes the <em>cite</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _cite() throws IOException {
        return this.close("</cite>");
    }

    /**
     * Opens the <em>dfn</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_dfn()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dfn() throws IOException {
        out.write("<dfn>");
        stack.push("</dfn>");
        return this;
    }

    /**
     * Opens the <em>dfn</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_dfn()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dfn(Attributes attrs) throws IOException {
        out.write("<dfn");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</dfn>");
        return this;
    }

	/**
     * Closes the <em>dfn</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _dfn() throws IOException {
        return this.close("</dfn>");
    }

    /**
     * Opens the <em>code</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_code()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas code() throws IOException {
        out.write("<code>");
        stack.push("</code>");
        return this;
    }

    /**
     * Opens the <em>code</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_code()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas code(Attributes attrs) throws IOException {
        out.write("<code");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</code>");
        return this;
    }

	/**
     * Closes the <em>code</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _code() throws IOException {
        return this.close("</code>");
    }

    /**
     * Opens the <em>samp</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_samp()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas samp() throws IOException {
        out.write("<samp>");
        stack.push("</samp>");
        return this;
    }

    /**
     * Opens the <em>samp</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_samp()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas samp(Attributes attrs) throws IOException {
        out.write("<samp");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</samp>");
        return this;
    }

	/**
     * Closes the <em>samp</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _samp() throws IOException {
        return this.close("</samp>");
    }

    /**
     * Opens the <em>kbd</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_kbd()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas kbd() throws IOException {
        out.write("<kbd>");
        stack.push("</kbd>");
        return this;
    }

    /**
     * Opens the <em>kbd</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_kbd()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas kbd(Attributes attrs) throws IOException {
        out.write("<kbd");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</kbd>");
        return this;
    }

	/**
     * Closes the <em>kbd</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _kbd() throws IOException {
        return this.close("</kbd>");
    }

    /**
     * Opens the <em>var</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_var()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas var() throws IOException {
        out.write("<var>");
        stack.push("</var>");
        return this;
    }

    /**
     * Opens the <em>var</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_var()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas var(Attributes attrs) throws IOException {
        out.write("<var");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</var>");
        return this;
    }

	/**
     * Closes the <em>var</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _var() throws IOException {
        return this.close("</var>");
    }

    /**
     * Opens the <em>address</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_address()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas address() throws IOException {
        out.write("<address>");
        stack.push("</address>");
        return this;
    }

    /**
     * Opens the <em>address</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_address()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas address(Attributes attrs) throws IOException {
        out.write("<address");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</address>");
        return this;
    }

	/**
     * Closes the <em>address</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _address() throws IOException {
        return this.close("</address>");
    }

    /**
     * Opens the (deprecated) <em>applet</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_applet()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas applet() throws IOException {
        out.write("<applet>");
        stack.push("</applet>");
        return this;
    }

    /**
     * Opens the <em>applet</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_applet()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>codebase<dd>optional base URI for applet
     * <dt>code<dd>applet class file
     * <dt>name<dd>allows applets to find each other
     * <dt>archive<dd>comma separated archive list
     * <dt>object<dd>serialized applet file
     * <dt>width<dd>initial width
     * <dt>height<dd>initial height
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>alt<dd>short description
     * <dt>align<dd>vertical or horizontal alignment
     * <dt>hspace<dd>horizontal gutter
     * <dt>vspace<dd>vertical gutter
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas applet(Attributes attrs) throws IOException {
        out.write("<applet");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</applet>");
        return this;
    }

	/**
     * Closes the <em>applet</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _applet() throws IOException {
        return this.close("</applet>");
    }

    /**
     * Opens the <em>area</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas area() throws IOException {
        out.write("<area>");
        stack.push("</area>");
        return this;
    }

    /**
     * Opens the <em>area</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>coords<dd>comma separated list of lengths
     * <dt>shape<dd>controls interpretation of coords
     * <dt>nohref<dd>this region has no action
     * <dt>name<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>shape<dd>controls interpretation of coords
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>the element got the focus
     * <dt>onblur<dd>the element lost the focus
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>target<dd>render in this frame
     * <dt>tabindex<dd>position in tabbing order
     * <dt>accesskey<dd>accessibility key character
     * <dt>href<dd>URI for linked resource
     * <dt>alt<dd>short description
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas area(Attributes attrs) throws IOException {
        out.write("<area");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</area>");
        return this;
    }

	/**
     * Closes the <em>area</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _area() throws IOException {
        return this.close("</area>");
    }

    /**
     * Opens the <em>map</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_map()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas map() throws IOException {
        out.write("<map>");
        stack.push("</map>");
        return this;
    }

    /**
     * Opens the <em>map</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_map()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>for reference by usemap
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas map(Attributes attrs) throws IOException {
        out.write("<map");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</map>");
        return this;
    }

	/**
     * Closes the <em>map</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _map() throws IOException {
        return this.close("</map>");
    }

    /**
     * Opens the <em>b</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_b()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas b() throws IOException {
        out.write("<b>");
        stack.push("</b>");
        return this;
    }

    /**
     * Opens the <em>b</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_b()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas b(Attributes attrs) throws IOException {
        out.write("<b");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</b>");
        return this;
    }

	/**
     * Closes the <em>b</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _b() throws IOException {
        return this.close("</b>");
    }

    /**
     * Opens the <em>tt</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_tt()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tt() throws IOException {
        out.write("<tt>");
        stack.push("</tt>");
        return this;
    }

    /**
     * Opens the <em>tt</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_tt()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tt(Attributes attrs) throws IOException {
        out.write("<tt");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</tt>");
        return this;
    }

	/**
     * Closes the <em>tt</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _tt() throws IOException {
        return this.close("</tt>");
    }

    /**
     * Opens the <em>i</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_i()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas i() throws IOException {
        out.write("<i>");
        stack.push("</i>");
        return this;
    }

    /**
     * Opens the <em>i</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_i()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas i(Attributes attrs) throws IOException {
        out.write("<i");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</i>");
        return this;
    }

	/**
     * Closes the <em>i</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _i() throws IOException {
        return this.close("</i>");
    }

    /**
     * Opens the <em>big</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_big()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas big() throws IOException {
        out.write("<big>");
        stack.push("</big>");
        return this;
    }

    /**
     * Opens the <em>big</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_big()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas big(Attributes attrs) throws IOException {
        out.write("<big");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</big>");
        return this;
    }

	/**
     * Closes the <em>big</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _big() throws IOException {
        return this.close("</big>");
    }

    /**
     * Opens the <em>small</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_small()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas small() throws IOException {
        out.write("<small>");
        stack.push("</small>");
        return this;
    }

    /**
     * Opens the <em>small</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_small()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas small(Attributes attrs) throws IOException {
        out.write("<small");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</small>");
        return this;
    }

	/**
     * Closes the <em>small</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _small() throws IOException {
        return this.close("</small>");
    }

    /**
     * Opens the (deprecated) <em>strike</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_strike()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas strike() throws IOException {
        out.write("<strike>");
        stack.push("</strike>");
        return this;
    }

    /**
     * Opens the <em>strike</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_strike()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas strike(Attributes attrs) throws IOException {
        out.write("<strike");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</strike>");
        return this;
    }

	/**
     * Closes the <em>strike</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _strike() throws IOException {
        return this.close("</strike>");
    }

    /**
     * Opens the (deprecated) <em>s</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_s()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas s() throws IOException {
        out.write("<s>");
        stack.push("</s>");
        return this;
    }

    /**
     * Opens the <em>s</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_s()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas s(Attributes attrs) throws IOException {
        out.write("<s");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</s>");
        return this;
    }

	/**
     * Closes the <em>s</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _s() throws IOException {
        return this.close("</s>");
    }

    /**
     * Opens the (deprecated) <em>u</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_u()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas u() throws IOException {
        out.write("<u>");
        stack.push("</u>");
        return this;
    }

    /**
     * Opens the <em>u</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_u()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas u(Attributes attrs) throws IOException {
        out.write("<u");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</u>");
        return this;
    }

	/**
     * Closes the <em>u</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _u() throws IOException {
        return this.close("</u>");
    }

    /**
     * Opens the <em>base</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas base() throws IOException {
        out.write("<base>");
        stack.push("</base>");
        return this;
    }

    /**
     * Opens the <em>base</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>href<dd>URI that acts as base URI
     * <dt>target<dd>render in this frame
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas base(Attributes attrs) throws IOException {
        out.write("<base");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</base>");
        return this;
    }

	/**
     * Closes the <em>base</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _base() throws IOException {
        return this.close("</base>");
    }

    /**
     * Opens the (deprecated) <em>basefont</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas basefont() throws IOException {
        out.write("<basefont>");
        stack.push("</basefont>");
        return this;
    }

    /**
     * Opens the <em>basefont</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>size<dd>base font size for FONT elements
     * <dt>color<dd>text color
     * <dt>face<dd>comma separated list of font names
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>
     * <dt>lang<dd>
     * <dt>dir<dd>
     * <dt>title<dd>
     * <dt>style<dd>
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas basefont(Attributes attrs) throws IOException {
        out.write("<basefont");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</basefont>");
        return this;
    }

	/**
     * Closes the <em>basefont</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _basefont() throws IOException {
        return this.close("</basefont>");
    }

    /**
     * Opens the (deprecated) <em>font</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_font()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas font() throws IOException {
        out.write("<font>");
        stack.push("</font>");
        return this;
    }

    /**
     * Opens the <em>font</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_font()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>size<dd>[+|-]nn e.g. size=+1, size=4
     * <dt>color<dd>text color
     * <dt>face<dd>comma separated list of font names
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas font(Attributes attrs) throws IOException {
        out.write("<font");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</font>");
        return this;
    }

	/**
     * Closes the <em>font</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _font() throws IOException {
        return this.close("</font>");
    }

    /**
     * Opens the <em>bdo</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_bdo()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas bdo() throws IOException {
        out.write("<bdo>");
        stack.push("</bdo>");
        return this;
    }

    /**
     * Opens the <em>bdo</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_bdo()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>dir<dd>directionality
     * <dt>lang<dd>language code
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas bdo(Attributes attrs) throws IOException {
        out.write("<bdo");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</bdo>");
        return this;
    }

	/**
     * Closes the <em>bdo</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _bdo() throws IOException {
        return this.close("</bdo>");
    }

    /**
     * Opens the <em>body</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_body()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas body() throws IOException {
        out.write("<body>");
        stack.push("</body>");
        return this;
    }

    /**
     * Opens the <em>body</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_body()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>background<dd>texture tile for document background
     * <dt>text<dd>document text color
     * <dt>link<dd>color of links
     * <dt>vlink<dd>color of visited links
     * <dt>alink<dd>color of selected links
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>bgcolor<dd>document background color
     * <dt>onload<dd>the document has been loaded
     * <dt>onunload<dd>the document has been removed
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas body(Attributes attrs) throws IOException {
        out.write("<body");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</body>");
        return this;
    }

	/**
     * Closes the <em>body</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _body() throws IOException {
        return this.close("</body>");
    }

    /**
     * Opens the <em>br</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas br() throws IOException {
        out.write("<br>");
        stack.push("</br>");
        return this;
    }

    /**
     * Opens the <em>br</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>clear<dd>control of text flow
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas br(Attributes attrs) throws IOException {
        out.write("<br");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</br>");
        return this;
    }

	/**
     * Closes the <em>br</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _br() throws IOException {
        return this.close("</br>");
    }

    /**
     * Opens the <em>button</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_button()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas button() throws IOException {
        out.write("<button>");
        stack.push("</button>");
        return this;
    }

    /**
     * Opens the <em>button</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_button()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>
     * <dt>value<dd>sent to server when submitted
     * <dt>type<dd>for use as form button
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>disabled<dd>unavailable in this context
     * <dt>accesskey<dd>accessibility key character
     * <dt>tabindex<dd>position in tabbing order
     * <dt>onblure<dd>
     * <dt>onfocus<dd>the element got the focus
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas button(Attributes attrs) throws IOException {
        out.write("<button");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</button>");
        return this;
    }

	/**
     * Closes the <em>button</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _button() throws IOException {
        return this.close("</button>");
    }

    /**
     * Opens the <em>caption</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_caption()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas caption() throws IOException {
        out.write("<caption>");
        stack.push("</caption>");
        return this;
    }

    /**
     * Opens the <em>caption</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_caption()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>align<dd>relative to table
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>lang<dd>language code
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas caption(Attributes attrs) throws IOException {
        out.write("<caption");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</caption>");
        return this;
    }

	/**
     * Closes the <em>caption</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _caption() throws IOException {
        return this.close("</caption>");
    }

    /**
     * Opens the (deprecated) <em>center</em> tag, without any attributes.
     * This tag does not support any attributes.
     *
     * <p>Close this tag by calling {@link #_center()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas center() throws IOException {
        out.write("<center>");
        stack.push("</center>");
        return this;
    }

    /**
     * Opens the <em>col</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas col() throws IOException {
        out.write("<col>");
        stack.push("</col>");
        return this;
    }

    /**
     * Opens the <em>col</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>span<dd>COL attributes affect N columns
     * <dt>width<dd>column width specification
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * <dt>valign<dd>vertical alignment in cells
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas col(Attributes attrs) throws IOException {
        out.write("<col");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</col>");
        return this;
    }

	/**
     * Closes the <em>col</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _col() throws IOException {
        return this.close("</col>");
    }

    /**
     * Opens the <em>colgroup</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_colgroup()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas colgroup() throws IOException {
        out.write("<colgroup>");
        stack.push("</colgroup>");
        return this;
    }

    /**
     * Opens the <em>colgroup</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_colgroup()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>span<dd>default number of columns in group
     * <dt>width<dd>default width for enclosed COLs
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * <dt>valign<dd>vertical alignment in cells
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas colgroup(Attributes attrs) throws IOException {
        out.write("<colgroup");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</colgroup>");
        return this;
    }

	/**
     * Closes the <em>colgroup</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _colgroup() throws IOException {
        return this.close("</colgroup>");
    }

    /**
     * Opens the <em>dd</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_dd()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dd() throws IOException {
        out.write("<dd>");
        stack.push("</dd>");
        return this;
    }

    /**
     * Opens the <em>dd</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_dd()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>char<dd>
     * <dt>charoff<dd>
     * <dt>valign<dd>
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dd(Attributes attrs) throws IOException {
        out.write("<dd");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</dd>");
        return this;
    }

	/**
     * Closes the <em>dd</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _dd() throws IOException {
        return this.close("</dd>");
    }

    /**
     * Opens the <em>dl</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_dl()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dl() throws IOException {
        out.write("<dl>");
        stack.push("</dl>");
        return this;
    }

    /**
     * Opens the <em>dl</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_dl()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>char<dd>
     * <dt>charoff<dd>
     * <dt>valign<dd>
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dl(Attributes attrs) throws IOException {
        out.write("<dl");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</dl>");
        return this;
    }

	/**
     * Closes the <em>dl</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _dl() throws IOException {
        return this.close("</dl>");
    }

    /**
     * Opens the <em>dt</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_dt()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dt() throws IOException {
        out.write("<dt>");
        stack.push("</dt>");
        return this;
    }

    /**
     * Opens the <em>dt</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_dt()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>char<dd>
     * <dt>charoff<dd>
     * <dt>valign<dd>
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas dt(Attributes attrs) throws IOException {
        out.write("<dt");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</dt>");
        return this;
    }

	/**
     * Closes the <em>dt</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _dt() throws IOException {
        return this.close("</dt>");
    }

    /**
     * Opens the <em>del</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_del()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas del() throws IOException {
        out.write("<del>");
        stack.push("</del>");
        return this;
    }

    /**
     * Opens the <em>del</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_del()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>cite<dd>info on reason for change
     * <dt>datetime<dd>date and time of change
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas del(Attributes attrs) throws IOException {
        out.write("<del");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</del>");
        return this;
    }

	/**
     * Closes the <em>del</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _del() throws IOException {
        return this.close("</del>");
    }

    /**
     * Opens the <em>ins</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_ins()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ins() throws IOException {
        out.write("<ins>");
        stack.push("</ins>");
        return this;
    }

    /**
     * Opens the <em>ins</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_ins()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>cite<dd>info on reason for change
     * <dt>datetime<dd>date and time of change
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ins(Attributes attrs) throws IOException {
        out.write("<ins");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</ins>");
        return this;
    }

	/**
     * Closes the <em>ins</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _ins() throws IOException {
        return this.close("</ins>");
    }

    /**
     * Opens the (deprecated) <em>dir</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_dir()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas dir() throws IOException {
        out.write("<dir>");
        stack.push("</dir>");
        return this;
    }

    /**
     * Opens the <em>dir</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_dir()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas dir(Attributes attrs) throws IOException {
        out.write("<dir");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</dir>");
        return this;
    }

	/**
     * Closes the <em>dir</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _dir() throws IOException {
        return this.close("</dir>");
    }

    /**
     * Opens the (deprecated) <em>menu</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_menu()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas menu() throws IOException {
        out.write("<menu>");
        stack.push("</menu>");
        return this;
    }

    /**
     * Opens the <em>menu</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_menu()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas menu(Attributes attrs) throws IOException {
        out.write("<menu");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</menu>");
        return this;
    }

	/**
     * Closes the <em>menu</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _menu() throws IOException {
        return this.close("</menu>");
    }

    /**
     * Opens the <em>div</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_div()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas div() throws IOException {
        out.write("<div>");
        stack.push("</div>");
        return this;
    }

    /**
     * Opens the <em>div</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_div()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas div(Attributes attrs) throws IOException {
        out.write("<div");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</div>");
        return this;
    }

	/**
     * Closes the <em>div</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _div() throws IOException {
        return this.close("</div>");
    }

    /**
     * Opens the <em>span</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_span()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas span() throws IOException {
        out.write("<span>");
        stack.push("</span>");
        return this;
    }

    /**
     * Opens the <em>span</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_span()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas span(Attributes attrs) throws IOException {
        out.write("<span");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</span>");
        return this;
    }

	/**
     * Closes the <em>span</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _span() throws IOException {
        return this.close("</span>");
    }

    /**
     * Opens the <em>fieldset</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_fieldset()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas fieldset() throws IOException {
        out.write("<fieldset>");
        stack.push("</fieldset>");
        return this;
    }

    /**
     * Opens the <em>fieldset</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_fieldset()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>align<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>accesskey<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas fieldset(Attributes attrs) throws IOException {
        out.write("<fieldset");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</fieldset>");
        return this;
    }

	/**
     * Closes the <em>fieldset</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _fieldset() throws IOException {
        return this.close("</fieldset>");
    }

    /**
     * Opens the <em>form</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_form()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas form() throws IOException {
        out.write("<form>");
        stack.push("</form>");
        return this;
    }

    /**
     * Opens the <em>form</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_form()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>action<dd>server-side form handler
     * <dt>method<dd>HTTP method used to submit the form
     * <dt>enctype<dd>
     * <dt>accept-charset<dd>list of supported charsets
     * <dt>accept<dd>
     * <dt>name<dd>name of form for scripting
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>target<dd>render in this frame
     * <dt>onsubmit<dd>the form was submitted
     * <dt>onreset<dd>the form was reset
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas form(Attributes attrs) throws IOException {
        out.write("<form");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</form>");
        return this;
    }

	/**
     * Closes the <em>form</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _form() throws IOException {
        return this.close("</form>");
    }

    /**
     * Opens the <em>frame</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas frame() throws IOException {
        out.write("<frame>");
        stack.push("</frame>");
        return this;
    }

    /**
     * Opens the <em>frame</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>name of frame for targetting
     * <dt>longdesc<dd>link to long description (complements title)
     * <dt>src<dd>source of frame content
     * <dt>noresize<dd>allow users to resize frames?
     * <dt>scrolling<dd>scrollbar or none
     * <dt>frameborder<dd>request frame borders?
     * <dt>margin-width<dd>
     * <dt>margin-height<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas frame(Attributes attrs) throws IOException {
        out.write("<frame");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</frame>");
        return this;
    }

	/**
     * Closes the <em>frame</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _frame() throws IOException {
        return this.close("</frame>");
    }

    /**
     * Opens the <em>frameset</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_frameset()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas frameset() throws IOException {
        out.write("<frameset>");
        stack.push("</frameset>");
        return this;
    }

    /**
     * Opens the <em>frameset</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_frameset()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>rows<dd>list of lengths, default: 100% (1 row)
     * <dt>cols<dd>list of lengths, default: 100% (1 col)
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onload<dd>all the frames have been loaded
     * <dt>onunload<dd>all the frames have been removed
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas frameset(Attributes attrs) throws IOException {
        out.write("<frameset");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</frameset>");
        return this;
    }

	/**
     * Closes the <em>frameset</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _frameset() throws IOException {
        return this.close("</frameset>");
    }

    /**
     * Opens the <em>h1</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h1()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h1() throws IOException {
        out.write("<h1>");
        stack.push("</h1>");
        return this;
    }

    /**
     * Opens the <em>h1</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h1()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h1(Attributes attrs) throws IOException {
        out.write("<h1");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h1>");
        return this;
    }

	/**
     * Closes the <em>h1</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h1() throws IOException {
        return this.close("</h1>");
    }

    /**
     * Opens the <em>h2</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h2()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h2() throws IOException {
        out.write("<h2>");
        stack.push("</h2>");
        return this;
    }

    /**
     * Opens the <em>h2</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h2()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h2(Attributes attrs) throws IOException {
        out.write("<h2");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h2>");
        return this;
    }

	/**
     * Closes the <em>h2</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h2() throws IOException {
        return this.close("</h2>");
    }

    /**
     * Opens the <em>h3</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h3()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h3() throws IOException {
        out.write("<h3>");
        stack.push("</h3>");
        return this;
    }

    /**
     * Opens the <em>h3</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h3()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h3(Attributes attrs) throws IOException {
        out.write("<h3");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h3>");
        return this;
    }

	/**
     * Closes the <em>h3</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h3() throws IOException {
        return this.close("</h3>");
    }

    /**
     * Opens the <em>h4</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h4()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h4() throws IOException {
        out.write("<h4>");
        stack.push("</h4>");
        return this;
    }

    /**
     * Opens the <em>h4</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h4()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h4(Attributes attrs) throws IOException {
        out.write("<h4");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h4>");
        return this;
    }

	/**
     * Closes the <em>h4</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h4() throws IOException {
        return this.close("</h4>");
    }

    /**
     * Opens the <em>h5</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h5()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h5() throws IOException {
        out.write("<h5>");
        stack.push("</h5>");
        return this;
    }

    /**
     * Opens the <em>h5</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h5()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h5(Attributes attrs) throws IOException {
        out.write("<h5");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h5>");
        return this;
    }

	/**
     * Closes the <em>h5</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h5() throws IOException {
        return this.close("</h5>");
    }

    /**
     * Opens the <em>h6</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_h6()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h6() throws IOException {
        out.write("<h6>");
        stack.push("</h6>");
        return this;
    }

    /**
     * Opens the <em>h6</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_h6()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas h6(Attributes attrs) throws IOException {
        out.write("<h6");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</h6>");
        return this;
    }

	/**
     * Closes the <em>h6</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _h6() throws IOException {
        return this.close("</h6>");
    }

    /**
     * Opens the <em>head</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_head()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas head() throws IOException {
        out.write("<head>");
        stack.push("</head>");
        return this;
    }

    /**
     * Opens the <em>head</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_head()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>profile<dd>named dictionary of meta info
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas head(Attributes attrs) throws IOException {
        out.write("<head");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</head>");
        return this;
    }

	/**
     * Closes the <em>head</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _head() throws IOException {
        return this.close("</head>");
    }

    /**
     * Opens the <em>hr</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas hr() throws IOException {
        out.write("<hr>");
        stack.push("</hr>");
        return this;
    }

    /**
     * Opens the <em>hr</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>align<dd>
     * <dt>noshade<dd>
     * <dt>size<dd>
     * <dt>width<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas hr(Attributes attrs) throws IOException {
        out.write("<hr");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</hr>");
        return this;
    }

	/**
     * Closes the <em>hr</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _hr() throws IOException {
        return this.close("</hr>");
    }

    /**
     * Opens the <em>html</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_html()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas html() throws IOException {
        out.write("<html>");
        stack.push("</html>");
        return this;
    }

    /**
     * Opens the <em>html</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_html()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>version<dd>Constant
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas html(Attributes attrs) throws IOException {
        out.write("<html");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</html>");
        return this;
    }

	/**
     * Closes the <em>html</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _html() throws IOException {
        return this.close("</html>");
    }

    /**
     * Opens the <em>iframe</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_iframe()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas iframe() throws IOException {
        out.write("<iframe>");
        stack.push("</iframe>");
        return this;
    }

    /**
     * Opens the <em>iframe</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_iframe()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>longdesc<dd>link to long description (complements title)
     * <dt>name<dd>name of frame for targetting
     * <dt>width<dd>frame width
     * <dt>height<dd>frame height
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>src<dd>source of frame content
     * <dt>frameborder<dd>request frame borders?
     * <dt>marginwidth<dd>margin widths in pixels
     * <dt>marginheight<dd>margin height in pixels
     * <dt>scrolling<dd>scrollbar or none
     * <dt>align<dd>vertical or horizontal alignment
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas iframe(Attributes attrs) throws IOException {
        out.write("<iframe");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</iframe>");
        return this;
    }

	/**
     * Closes the <em>iframe</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _iframe() throws IOException {
        return this.close("</iframe>");
    }

    /**
     * Opens the <em>img</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas img() throws IOException {
        out.write("<img>");
        stack.push("</img>");
        return this;
    }

    /**
     * Opens the <em>img</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>src<dd>URI of image to embed
     * <dt>longdesc<dd>link to long description (complements alt)
     * <dt>name<dd>name of image for scripting
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>alt<dd>short description
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>ismap<dd>use server-side image map
     * <dt>usemap<dd>use client-side image map
     * <dt>align<dd>vertical or horizontal alignment
     * <dt>width<dd>override width
     * <dt>height<dd>override height
     * <dt>border<dd>link border width
     * <dt>hspace<dd>horizontal gutter
     * <dt>vspace<dd>vertical gutter
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas img(Attributes attrs) throws IOException {
        out.write("<img");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</img>");
        return this;
    }

	/**
     * Closes the <em>img</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _img() throws IOException {
        return this.close("</img>");
    }

    /**
     * Opens the <em>input</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas input() throws IOException {
        out.write("<input>");
        stack.push("</input>");
        return this;
    }

    /**
     * Opens the <em>input</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>type<dd>what kind of widget is needed
     * <dt>name<dd>submit as part of form
     * <dt>value<dd>required for radio and checkboxes
     * <dt>size<dd>specific to each type of field
     * <dt>maxlength<dd>max chars for text fields
     * <dt>checked<dd>for radio buttons and check boxes
     * <dt>src<dd>for fields with images
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>alt<dd>short description
     * <dt>align<dd>vertical or horizontal alignment
     * <dt>accept<dd>list of MIME types for file upload
     * <dt>disabled<dd>unavailable in this context
     * <dt>tabindex<dd>position in tabbing order
     * <dt>accesskey<dd>accessibility key character
     * <dt>usemap<dd>use client-side image map
     * <dt>ismap<dd>use server-side image map
     * <dt>readonly<dd>for text and passwd
     * <dt>onfocus<dd>the element got the focus
     * <dt>onblur<dd>the element lost the focus
     * <dt>onselect<dd>some text was selected
     * <dt>onchange<dd>the element value was changed
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas input(Attributes attrs) throws IOException {
        out.write("<input");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</input>");
        return this;
    }

	/**
     * Closes the <em>input</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _input() throws IOException {
        return this.close("</input>");
    }

    /**
     * Opens the (deprecated) <em>isindex</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas isindex() throws IOException {
        out.write("<isindex>");
        stack.push("</isindex>");
        return this;
    }

    /**
     * Opens the <em>isindex</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>prompt<dd>prompt message
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     * @deprecated this tag is deprecated in HTML 4.0.
     */
    @Deprecated
    public HtmlCanvas isindex(Attributes attrs) throws IOException {
        out.write("<isindex");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</isindex>");
        return this;
    }

	/**
     * Closes the <em>isindex</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _isindex() throws IOException {
        return this.close("</isindex>");
    }

    /**
     * Opens the <em>label</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_label()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas label() throws IOException {
        out.write("<label>");
        stack.push("</label>");
        return this;
    }

    /**
     * Opens the <em>label</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_label()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>for<dd>matches field ID value
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>accesskey<dd>accessibility key character
     * <dt>onfocus<dd>the element got the focus
     * <dt>onblur<dd>the element lost the focus
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas label(Attributes attrs) throws IOException {
        out.write("<label");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</label>");
        return this;
    }

	/**
     * Closes the <em>label</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _label() throws IOException {
        return this.close("</label>");
    }

    /**
     * Opens the <em>legend</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_legend()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas legend() throws IOException {
        out.write("<legend>");
        stack.push("</legend>");
        return this;
    }

    /**
     * Opens the <em>legend</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_legend()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>for<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>accesskey<dd>accessibility key character
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas legend(Attributes attrs) throws IOException {
        out.write("<legend");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</legend>");
        return this;
    }

	/**
     * Closes the <em>legend</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _legend() throws IOException {
        return this.close("</legend>");
    }

    /**
     * Opens the <em>li</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_li()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas li() throws IOException {
        out.write("<li>");
        stack.push("</li>");
        return this;
    }

    /**
     * Opens the <em>li</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_li()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>type<dd>list item style
     * <dt>start<dd>
     * <dt>value<dd>reset sequence number
     * <dt>compact<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas li(Attributes attrs) throws IOException {
        out.write("<li");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</li>");
        return this;
    }

	/**
     * Closes the <em>li</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _li() throws IOException {
        return this.close("</li>");
    }

    /**
     * Opens the <em>ol</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_ol()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ol() throws IOException {
        out.write("<ol>");
        stack.push("</ol>");
        return this;
    }

    /**
     * Opens the <em>ol</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_ol()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>type<dd>numbering style
     * <dt>start<dd>starting sequence number
     * <dt>value<dd>
     * <dt>compact<dd>reduced interitem spacing
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ol(Attributes attrs) throws IOException {
        out.write("<ol");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</ol>");
        return this;
    }

	/**
     * Closes the <em>ol</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _ol() throws IOException {
        return this.close("</ol>");
    }

    /**
     * Opens the <em>ul</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_ul()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ul() throws IOException {
        out.write("<ul>");
        stack.push("</ul>");
        return this;
    }

    /**
     * Opens the <em>ul</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_ul()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>type<dd>bullet style
     * <dt>start<dd>
     * <dt>value<dd>
     * <dt>compact<dd>reduced interitem spacing
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas ul(Attributes attrs) throws IOException {
        out.write("<ul");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</ul>");
        return this;
    }

	/**
     * Closes the <em>ul</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _ul() throws IOException {
        return this.close("</ul>");
    }

    /**
     * Opens the <em>link</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas link() throws IOException {
        out.write("<link>");
        stack.push("</link>");
        return this;
    }

    /**
     * Opens the <em>link</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onfocus<dd>
     * <dt>onblur<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>href<dd>URI for linked resource
     * <dt>hreflang<dd>language code
     * <dt>rel<dd>forward link types
     * <dt>rev<dd>reverse link types
     * <dt>target<dd>render in this frame
     * <dt>charset<dd>char encoding of linked resource
     * <dt>media<dd>for rendering on these media
     * <dt>type<dd>advisory content type
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas link(Attributes attrs) throws IOException {
        out.write("<link");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</link>");
        return this;
    }

	/**
     * Closes the <em>link</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _link() throws IOException {
        return this.close("</link>");
    }

    /**
     * Opens the <em>meta</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas meta() throws IOException {
        out.write("<meta>");
        stack.push("</meta>");
        return this;
    }

    /**
     * Opens the <em>meta</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>metainformation name
     * <dt>content<dd>associated information
     * <dt>scheme<dd>select form of content
     * <dt>http-equiv<dd>HTTP response header name
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas meta(Attributes attrs) throws IOException {
        out.write("<meta");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</meta>");
        return this;
    }

	/**
     * Closes the <em>meta</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _meta() throws IOException {
        return this.close("</meta>");
    }

    /**
     * Opens the <em>noframes</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_noframes()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas noframes() throws IOException {
        out.write("<noframes>");
        stack.push("</noframes>");
        return this;
    }

    /**
     * Opens the <em>noframes</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_noframes()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas noframes(Attributes attrs) throws IOException {
        out.write("<noframes");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</noframes>");
        return this;
    }

	/**
     * Closes the <em>noframes</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _noframes() throws IOException {
        return this.close("</noframes>");
    }

    /**
     * Opens the <em>noscript</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_noscript()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas noscript() throws IOException {
        out.write("<noscript>");
        stack.push("</noscript>");
        return this;
    }

    /**
     * Opens the <em>noscript</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_noscript()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas noscript(Attributes attrs) throws IOException {
        out.write("<noscript");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</noscript>");
        return this;
    }

	/**
     * Closes the <em>noscript</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _noscript() throws IOException {
        return this.close("</noscript>");
    }

    /**
     * Opens the <em>object</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_object()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas object() throws IOException {
        out.write("<object>");
        stack.push("</object>");
        return this;
    }

    /**
     * Opens the <em>object</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_object()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>classid<dd>identifies an implementation
     * <dt>codebase<dd>base URI for classid, data, archive
     * <dt>codetype<dd>content type for code
     * <dt>data<dd>reference to object's data
     * <dt>type<dd>content type for data
     * <dt>archive<dd>space separated archive list
     * <dt>declare<dd>declare but don't instantiate flag
     * <dt>standby<dd>message to show while loading
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>tabindex<dd>position in tabbing order
     * <dt>usemap<dd>use client-side image map
     * <dt>name<dd>submit as part of form
     * <dt>align<dd>vertical or horizontal alignment
     * <dt>width<dd>override width
     * <dt>height<dd>override height
     * <dt>border<dd>link border width
     * <dt>hspace<dd>horizontal gutter
     * <dt>vspace<dd>vertical gutter
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas object(Attributes attrs) throws IOException {
        out.write("<object");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</object>");
        return this;
    }

	/**
     * Closes the <em>object</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _object() throws IOException {
        return this.close("</object>");
    }

    /**
     * Opens the <em>optgroup</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_optgroup()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas optgroup() throws IOException {
        out.write("<optgroup>");
        stack.push("</optgroup>");
        return this;
    }

    /**
     * Opens the <em>optgroup</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_optgroup()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>
     * <dt>label<dd>for use in hierarchical menus
     * <dt>size<dd>
     * <dt>multiple<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>disabled<dd>unavailable in this context
     * <dt>tabindex<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas optgroup(Attributes attrs) throws IOException {
        out.write("<optgroup");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</optgroup>");
        return this;
    }

	/**
     * Closes the <em>optgroup</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _optgroup() throws IOException {
        return this.close("</optgroup>");
    }

    /**
     * Opens the <em>option</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_option()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas option() throws IOException {
        out.write("<option>");
        stack.push("</option>");
        return this;
    }

    /**
     * Opens the <em>option</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_option()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>selected<dd>
     * <dt>value<dd>defaults to element content
     * <dt>label<dd>for use in hierarchical menus
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>disabled<dd>unavailable in this context
     * <dt>tabindex<dd>
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas option(Attributes attrs) throws IOException {
        out.write("<option");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</option>");
        return this;
    }

	/**
     * Closes the <em>option</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _option() throws IOException {
        return this.close("</option>");
    }

    /**
     * Opens the <em>select</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_select()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas select() throws IOException {
        out.write("<select>");
        stack.push("</select>");
        return this;
    }

    /**
     * Opens the <em>select</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_select()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>field name
     * <dt>size<dd>rows visible
     * <dt>multiple<dd>default is single selection
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>disabled<dd>unavailable in this context
     * <dt>tabindex<dd>position in tabbing order
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>onblur<dd>the element lost the focus
     * <dt>onchange<dd>the element value was changed
     * <dt>onfocus<dd>the element got the focus
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas select(Attributes attrs) throws IOException {
        out.write("<select");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</select>");
        return this;
    }

	/**
     * Closes the <em>select</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _select() throws IOException {
        return this.close("</select>");
    }

    /**
     * Opens the <em>p</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_p()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas p() throws IOException {
        out.write("<p>");
        stack.push("</p>");
        return this;
    }

    /**
     * Opens the <em>p</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_p()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>alt<dd>
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>align<dd>align, text alignment
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas p(Attributes attrs) throws IOException {
        out.write("<p");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</p>");
        return this;
    }

	/**
     * Closes the <em>p</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _p() throws IOException {
        return this.close("</p>");
    }

    /**
     * Opens the <em>param</em> tag, without any attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas param() throws IOException {
        out.write("<param>");
        stack.push("</param>");
        return this;
    }

    /**
     * Opens the <em>param</em> tag, with the specified attributes.
     *
     * <p>This tag must not be closed, an end tag is forbidden.
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>property name
     * <dt>value<dd>property value
     * <dt>valuetype<dd>How to interpret value
     * <dt>type<dd>content type for value when valuetype=ref
     * <dt>id<dd>document-wide unique id
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas param(Attributes attrs) throws IOException {
        out.write("<param");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</param>");
        return this;
    }

	/**
     * Closes the <em>param</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _param() throws IOException {
        return this.close("</param>");
    }

    /**
     * Opens the <em>pre</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_pre()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas pre() throws IOException {
        out.write("<pre>");
        stack.push("</pre>");
        return this;
    }

    /**
     * Opens the <em>pre</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_pre()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>width<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas pre(Attributes attrs) throws IOException {
        out.write("<pre");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</pre>");
        return this;
    }

	/**
     * Closes the <em>pre</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _pre() throws IOException {
        return this.close("</pre>");
    }

    /**
     * Opens the <em>blockquote</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_blockquote()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas blockquote() throws IOException {
        out.write("<blockquote>");
        stack.push("</blockquote>");
        return this;
    }

    /**
     * Opens the <em>blockquote</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_blockquote()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>cite<dd>URI for source document or msg
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas blockquote(Attributes attrs) throws IOException {
        out.write("<blockquote");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</blockquote>");
        return this;
    }

	/**
     * Closes the <em>blockquote</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _blockquote() throws IOException {
        return this.close("</blockquote>");
    }

    /**
     * Opens the <em>q</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_q()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas q() throws IOException {
        out.write("<q>");
        stack.push("</q>");
        return this;
    }

    /**
     * Opens the <em>q</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_q()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>cite<dd>URI for source document or msg
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas q(Attributes attrs) throws IOException {
        out.write("<q");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</q>");
        return this;
    }

	/**
     * Closes the <em>q</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _q() throws IOException {
        return this.close("</q>");
    }

    /**
     * Opens the <em>script</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_script()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas script() throws IOException {
        out.write("<script>");
        stack.push("</script>");
        return this;
    }

    /**
     * Opens the <em>script</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_script()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>src<dd>URI for an external script
     * <dt>type<dd>content type of script language
     * <dt>language<dd>predefined script language name
     * <dt>defer<dd>UA may defer execution of script
     * <dt>charset<dd>char encoding of linked resource
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas script(Attributes attrs) throws IOException {
        out.write("<script");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</script>");
        return this;
    }

	/**
     * Closes the <em>script</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _script() throws IOException {
        return this.close("</script>");
    }

    /**
     * Opens the <em>style</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_style()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas style() throws IOException {
        out.write("<style>");
        stack.push("</style>");
        return this;
    }

    /**
     * Opens the <em>style</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_style()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>type<dd>content type of style language
     * <dt>media<dd>designed for use with these media
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas style(Attributes attrs) throws IOException {
        out.write("<style");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</style>");
        return this;
    }

	/**
     * Closes the <em>style</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _style() throws IOException {
        return this.close("</style>");
    }

    /**
     * Opens the <em>sub</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_sub()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas sub() throws IOException {
        out.write("<sub>");
        stack.push("</sub>");
        return this;
    }

    /**
     * Opens the <em>sub</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_sub()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas sub(Attributes attrs) throws IOException {
        out.write("<sub");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</sub>");
        return this;
    }

	/**
     * Closes the <em>sub</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _sub() throws IOException {
        return this.close("</sub>");
    }

    /**
     * Opens the <em>sup</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_sup()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas sup() throws IOException {
        out.write("<sup>");
        stack.push("</sup>");
        return this;
    }

    /**
     * Opens the <em>sup</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_sup()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas sup(Attributes attrs) throws IOException {
        out.write("<sup");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</sup>");
        return this;
    }

	/**
     * Closes the <em>sup</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _sup() throws IOException {
        return this.close("</sup>");
    }

    /**
     * Opens the <em>textarea</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_textarea()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas textarea() throws IOException {
        out.write("<textarea>");
        stack.push("</textarea>");
        return this;
    }

    /**
     * Opens the <em>textarea</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_textarea()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>name<dd>
     * <dt>rows<dd>
     * <dt>cols<dd>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>readonly<dd>
     * <dt>disabled<dd>unavailable in this context
     * <dt>tabindex<dd>position in tabbing order
     * <dt>accesskey<dd>accessibility key character
     * <dt>onfocus<dd>the element got the focus
     * <dt>onblur<dd>the element lost the focus
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>onchange<dd>the element value was changed
     * <dt>onselect<dd>some text was selected
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas textarea(Attributes attrs) throws IOException {
        out.write("<textarea");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</textarea>");
        return this;
    }

	/**
     * Closes the <em>textarea</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _textarea() throws IOException {
        return this.close("</textarea>");
    }

    /**
     * Opens the <em>title</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_title()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas title() throws IOException {
        out.write("<title>");
        stack.push("</title>");
        return this;
    }

    /**
     * Opens the <em>title</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_title()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas title(Attributes attrs) throws IOException {
        out.write("<title");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</title>");
        return this;
    }

	/**
     * Closes the <em>title</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _title() throws IOException {
        return this.close("</title>");
    }

    /**
     * Opens the <em>table</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_table()} (the end tag is required).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas table() throws IOException {
        out.write("<table>");
        stack.push("</table>");
        return this;
    }

    /**
     * Opens the <em>table</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_table()} (the end tag is required).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>summary<dd>purpose/structure for speech output
     * <dt>align<dd>table position relative to window
     * <dt>width<dd>table width
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>bgcolor<dd>background color for cells
     * <dt>frame<dd>which parts of frame to render
     * <dt>rules<dd>rulings between rows and cols
     * <dt>border<dd>controls frame width around table
     * <dt>cellspacing<dd>spacing between cells
     * <dt>cellpadding<dd>spacing within cells
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas table(Attributes attrs) throws IOException {
        out.write("<table");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</table>");
        return this;
    }

	/**
     * Closes the <em>table</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _table() throws IOException {
        return this.close("</table>");
    }

    /**
     * Opens the <em>tbody</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_tbody()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tbody() throws IOException {
        out.write("<tbody>");
        stack.push("</tbody>");
        return this;
    }

    /**
     * Opens the <em>tbody</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_tbody()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tbody(Attributes attrs) throws IOException {
        out.write("<tbody");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</tbody>");
        return this;
    }

	/**
     * Closes the <em>tbody</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _tbody() throws IOException {
        return this.close("</tbody>");
    }

    /**
     * Opens the <em>tfoot</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_tfoot()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tfoot() throws IOException {
        out.write("<tfoot>");
        stack.push("</tfoot>");
        return this;
    }

    /**
     * Opens the <em>tfoot</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_tfoot()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tfoot(Attributes attrs) throws IOException {
        out.write("<tfoot");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</tfoot>");
        return this;
    }

	/**
     * Closes the <em>tfoot</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _tfoot() throws IOException {
        return this.close("</tfoot>");
    }

    /**
     * Opens the <em>thead</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_thead()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas thead() throws IOException {
        out.write("<thead>");
        stack.push("</thead>");
        return this;
    }

    /**
     * Opens the <em>thead</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_thead()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas thead(Attributes attrs) throws IOException {
        out.write("<thead");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</thead>");
        return this;
    }

	/**
     * Closes the <em>thead</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _thead() throws IOException {
        return this.close("</thead>");
    }

    /**
     * Opens the <em>td</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_td()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas td() throws IOException {
        out.write("<td>");
        stack.push("</td>");
        return this;
    }

    /**
     * Opens the <em>td</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_td()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>headers<dd>list of id's for header cells
     * <dt>scope<dd>scope covered by header cells
     * <dt>abbr<dd>abbreviation for header cell
     * <dt>axis<dd>names groups of related headers
     * <dt>rowspan<dd>number of rows spanned by cell
     * <dt>colspan<dd>number of cols spanned by cell
     * <dt>nowrap<dd>suppress word wrap
     * <dt>width<dd>width for cell
     * <dt>height<dd>height for cell
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * <dt>bgcolor<dd>cell background color
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas td(Attributes attrs) throws IOException {
        out.write("<td");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</td>");
        return this;
    }

	/**
     * Closes the <em>td</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _td() throws IOException {
        return this.close("</td>");
    }

    /**
     * Opens the <em>th</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_th()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas th() throws IOException {
        out.write("<th>");
        stack.push("</th>");
        return this;
    }

    /**
     * Opens the <em>th</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_th()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>headers<dd>list of id's for header cells
     * <dt>scope<dd>scope covered by header cells
     * <dt>abbr<dd>abbreviation for header cell
     * <dt>axis<dd>names groups of related headers
     * <dt>rowspan<dd>number of rows spanned by cell
     * <dt>colspan<dd>number of cols spanned by cell
     * <dt>nowrap<dd>suppress word wrap
     * <dt>width<dd>width for cell
     * <dt>height<dd>height for cell
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * <dt>bgcolor<dd>cell background color
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas th(Attributes attrs) throws IOException {
        out.write("<th");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</th>");
        return this;
    }

	/**
     * Closes the <em>th</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _th() throws IOException {
        return this.close("</th>");
    }

    /**
     * Opens the <em>tr</em> tag, without any attributes.
     *
     * <p>Close this tag by calling {@link #_tr()} (the end tag is optional).
     *
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tr() throws IOException {
        out.write("<tr>");
        stack.push("</tr>");
        return this;
    }

    /**
     * Opens the <em>tr</em> tag, with the specified attributes.
     *
     * <p>Close this tag by calling {@link #_tr()} (the end tag is optional).
     *
     * <p>This tag supports the following attributes:
     * <dl>
     * <dt>id<dd>document-wide unique id
     * <dt>class<dd>space separated list of classes
     * <dt>lang<dd>language code
     * <dt>dir<dd>direction for weak/neutral text
     * <dt>title<dd>advisory title
     * <dt>style<dd>associated style info
     * <dt>onclick<dd>a pointer button was clicked
     * <dt>ondblclick<dd>a pointer button was double clicked
     * <dt>onmousedown<dd>a pointer button was pressed down
     * <dt>onmouseup<dd>a pointer button was released
     * <dt>onmouseover<dd>a pointer was moved onto
     * <dt>onmousemove<dd>a pointer was moved within
     * <dt>onmouseout<dd>a pointer was moved away
     * <dt>onkeypress<dd>a key was pressed and released
     * <dt>onkeydown<dd>a key was pressed down
     * <dt>onkeyup<dd>a key was released
     * <dt>align<dd>
     * <dt>valign<dd>vertical alignment in cells
     * <dt>char<dd>alignment char, e.g. char=':'
     * <dt>charoff<dd>offset for alignment char
     * <dt>bgcolor<dd>background color for row
     * </dl>
     *
     * @param attrs the {@link Attributes}, or <code>null</code> if none.
     * @return the receiver, this <code>HtmlCanvas</code> instance.
     * @throws IOException in case of an I/O error.
     */
    public HtmlCanvas tr(Attributes attrs) throws IOException {
        out.write("<tr");
        if (attrs != null)
            out.write(attrs.html());
        out.write('>');
        stack.push("</tr>");
        return this;
    }

	/**
     * Closes the <em>tr</em> tag.
     *
     * @throws IOException in case of an I/O error while writing the end tag.
     */
    public HtmlCanvas _tr() throws IOException {
        return this.close("</tr>");
    }
}
