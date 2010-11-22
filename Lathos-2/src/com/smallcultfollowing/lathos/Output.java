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
    private int embedDepth = 0;
    private final int maxEmbedDepth;

    public Output(LathosServer server, HttpServletRequest request, HttpServletResponse response, Writer out)
    {
        super(request, response, out);
        this.server = server;
        this.maxEmbedDepth = server.getMaxEmbedDepth();
    }

    public static final Output devNull = new Output(DevNullServer.instance, null, null, new Writer() {
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

    /**
     * Renders the object {@code obj}, using a relative link {@code link/fld}
     * 
     * @see RelativeLink
     */
    public void obj(Link link, String fld, Object obj) throws IOException
    {
        obj(new RelativeLink(link, fld), obj);
    }

    /**
     * Renders the object {@code obj}, using an index link {@code link/fld}
     * 
     * @see IndexLink
     */
    public void obj(Link link, int idx, Object obj) throws IOException
    {
        obj(new IndexLink(link, idx), obj);
    }

    /** Renders the object {@code obj}, using the link {@code link} */
    public void obj(final Link link, final Object obj) throws IOException
    {
        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert (canvas == Output.this);
                server.renderObjectSummary(Output.this, link, obj);
            }
        });
    }

    /**
     * Embeds the object {@code obj}, using an indexed link {@code link/idx}
     * 
     * @see #embed(Link, Object)
     * @see RelativeLink
     */
    public void embed(Link link, int idx, Object obj) throws IOException
    {
        embed(new IndexLink(link, idx), obj);
    }
    
    /**
     * Embeds the object {@code obj}, using a relative link {@code link/fld}
     * 
     * @see #embed(Link, Object)
     * @see RelativeLink
     */
    public void embed(Link link, String fld, Object obj) throws IOException
    {
        embed(new RelativeLink(link, fld), obj);
    }
    
    /** Renders the object {@code obj}, using the link {@code link} */
    public void embed(final Link link, final Object obj) throws IOException
    {
        if (embedDepth == maxEmbedDepth) {
            i().text("Maximum embed depth of "+maxEmbedDepth+" exceeded!")._i();
            return;
        }

        server.getDelegate().startEmbed(this, embedDepth, link, obj);
        embedDepth += 1;
        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert (canvas == Output.this);
                server.renderObjectDetails(Output.this, link, obj);
            }
        });
        embedDepth -= 1;
        server.getDelegate().endEmbed(this, embedDepth, link, obj);
    }

    /** Emit a link to {@code link}, if non-null */
    public Output a(Link link) throws IOException
    {
        if (link != null && link.isValid()) {
            a(AttributesFactory.href(link.toString()));
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
            a(attrs.href(link.toString()));
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

    public void rawLine(String string) throws IOException
    {
        out.write(string);
        out.write("\n");
    }
}
