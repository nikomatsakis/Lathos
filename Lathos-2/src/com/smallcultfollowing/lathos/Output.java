package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.Writer;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.rendersnake.Attributes;
import org.rendersnake.AttributesFactory;
import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;

public class Output
    extends HtmlCanvas
{
    public final LathosServer server;

    public Output(LathosServer server, HttpServletRequest request, HttpServletResponse response, Writer out)
    {
        super(request, response, out);
        this.server = server;
    }

    public Output(LathosServer server, Writer output)
    {
        super(output);
        this.server = server;
    }

    public static final Output devNull = new Output(DevNullServer.instance, new Writer() {
        @Override
        public void write(char[] cbuf, int off, int len) throws IOException
        {
        }

        @Override
        public void flush() throws IOException
        {
        }

        @Override
        public void close() throws IOException
        {
        }
    });

    /**
     * Renders the object {@code obj}, creating a default link for it via
     * {@link LathosServer#defaultLink(Object)}
     */
    public void obj(Object obj) throws IOException
    {
        Link link = server.defaultLink(obj);
        obj(link, obj);
    }
    
    /** Renders the object {@code obj}, using a relative link {@code link/fld} */
    public void obj(Link link, String fld, Object obj) throws IOException
    {
        obj(new RelativeLink(link, fld), obj);        
    }

    /** Renders the object {@code obj}, using the link {@code link} */
    public void obj(final Link link, final Object obj) throws IOException
    {
        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert(canvas == Output.this);
                server.renderObjectSummary(Output.this, link, obj);
            }
        });
    }

    /** Renders the object {@code obj}, using the link {@code link} */
    public void embed(final Link link, final Object obj) throws IOException
    {
        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert(canvas == Output.this);
                server.renderObjectDetails(Output.this, link, obj);
            }
        });
    }

    /** Emit a link to {@code link}, if non-null */
    public Output a(Link link) throws IOException
    {
        if (link != null && link.isValid()) {
            StringBuilder sb = new StringBuilder();
            link.appendUrlString(sb);
            a(AttributesFactory.href(sb.toString()));
        }
        return this;
    }

    /**
     * Emit a link to {@code link}, if non-null, with additional attributes
     * {@code attrs}
     */
    public Output a(Link link, Attributes attrs) throws IOException
    {
        if (link != null && link.isValid()) {
            StringBuilder sb = new StringBuilder();
            link.appendUrlString(sb);
            a(attrs.href(sb.toString()));
        }
        return this;
    }

    /** Close a link to {@code link}, if non-null */
    public Output _a(Link link) throws IOException
    {
        if (link != null && link.isValid()) {
            _a();
        }
        return this;
    }
}
