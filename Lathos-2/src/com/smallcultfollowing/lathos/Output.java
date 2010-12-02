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
        final Page page = server.asPage(obj);
        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert (canvas == Output.this);
                page.renderSummary(Output.this, link);
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
            i().text("Maximum embed depth of " + maxEmbedDepth + " exceeded!")._i();
            return;
        }

        final Page page = server.asPage(obj);
        server.getDelegate().startEmbed(this, embedDepth, link, page);

        if (page instanceof Page.Titled) {
            Page.Titled titled = (Page.Titled) page;
            server.getDelegate().startEmbedTitle(this, embedDepth, link, titled);
            titled.renderTitle(Output.this, link);
            server.getDelegate().endEmbedTitle(this, embedDepth, link, titled);
        }

        server.getDelegate().startEmbedContent(this, embedDepth, link, page);
        embedDepth += 1;

        render(new Renderable() {
            @Override
            public void renderOn(HtmlCanvas canvas) throws IOException
            {
                assert (canvas == Output.this);
                if (page instanceof Page.Detailed) {
                    ((Page.Detailed) page).renderDetails(Output.this, link);
                } else {
                    page.renderSummary(Output.this, link);
                }
            }
        });

        embedDepth -= 1;
        server.getDelegate().endEmbedContent(this, embedDepth, link, page);

        server.getDelegate().endEmbed(this, embedDepth, link, page);
    }

    /**
     * Starts a subpage without a title. Must be matched with a call to link {
     * {@link #_subpage()}.
     */
    public void subpage() throws IOException
    {
        server.getDelegate().startSubpage(this, embedDepth);
        server.getDelegate().startSubpageContent(this, embedDepth);
        embedDepth += 1;
    }

    /**
     * Starts a subpage whose title is {@code title}. Must be matched with a
     * call to {@link #_subpage()}.
     */
    public void subpage(Link link, Object title) throws IOException
    {
        server.getDelegate().startSubpage(this, embedDepth);

        Page page = server.asPage(title);
        server.getDelegate().startSubpageTitle(this, embedDepth);
        page.renderSummary(this, link);
        server.getDelegate().endSubpageTitle(this, embedDepth);

        server.getDelegate().startSubpageContent(this, embedDepth);
        embedDepth += 1;
    }

    /**
     * Matched close call for {@link #subpage()} or
     * {@link #subpage(Link, Object)}
     */
    public void _subpage() throws IOException
    {
        embedDepth -= 1;
        server.getDelegate().endSubpageContent(this, embedDepth);
        server.getDelegate().endSubpage(this, embedDepth);
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
