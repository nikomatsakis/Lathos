package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.Writer;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import org.apache.commons.lang.StringEscapeUtils;

public abstract class DefaultServer
    implements LathosServer
{
    // Configuration keys:
    private static final String includeStaticPage = "includeStaticPage";
    
    private volatile ObjectSubst[] substs = new ObjectSubst[0];
    private final List<ObjectRenderer> renderers = new ArrayList<ObjectRenderer>();
    private final Map<String, RootPage> rootPages = new LinkedHashMap<String, RootPage>();
    private final IndexPage indexPage = new IndexPage();
    private LinkCache linkCache = null;

    /** Equivalent to {@code this(true, 10000)} */
    DefaultServer()
    {
        addRootPage(indexPage);
    }
    
    @Override
    public synchronized void setLinkCache(LinkCache aLinkCache)
    {
        if(linkCache != null) {
            removeRootPage(linkCache);
        }
        
        linkCache = aLinkCache;
        
        if(linkCache != null) {
            addRootPage(linkCache);
        }
    }
    
    @Override
    public synchronized Link defaultLink(Object obj)
    {
        if(linkCache == null)
            return null;
        
        return linkCache.makeLink(obj);
    }

    @Override
    public Object substitute(Object obj)
    {
        // I don't normally be in over-optimizing locking,
        // but as this is in the "fast path" of every log statement,
        // we use volatile statements here for synchronization.
        //
        // By reading substCount first, we guarantee that we will
        // see a "substs" array that contains *at least* that many
        // valid entries. We may actually read a more up-to-date
        // substs array, but the first "substCount" entries are always
        // the same as the array that was valid for the "substCount" we
        // read.
        ObjectSubst[] substs = this.substs;
        for (int i = 0; i < substs.length; i++) {
            obj = substs[i].substitute(obj);
        }

        return obj;
    }

    @Override
    public synchronized void addSubstitutionFilter(ObjectSubst subst)
    {
        ObjectSubst[] newSubsts = Arrays.copyOf(substs, substs.length + 1);
        newSubsts[substs.length] = subst;
        substs = newSubsts;
    }

    @Override
    public synchronized void renderObjectSummary(Output out, Link link, Object obj)
            throws IOException
    {
        for (int i = renderers.size() - 1; i >= 0; i--) {
            ObjectRenderer renderer = renderers.get(i);
            if (renderer.renderObjectSummary(obj, out, link))
                return;
        }

        // Bare-bones default behavior:
        if (obj == null) {
            out.text("null");
        } else {
            out.text(obj.toString());
        }
    }

    @Override
    public void renderObjectDetails(Output out, Link link, Object obj)
            throws IOException
    {
        for (int i = renderers.size() - 1; i >= 0; i--) {
            ObjectRenderer renderer = renderers.get(i);
            if (renderer.renderObjectDetails(obj, out, link))
                return;
        }
        
        renderObjectSummary(out, link, obj);
    }

    public Object derefPage(Object page, String link)
    {
        for (int i = renderers.size() - 1; i >= 0; i--) {
            ObjectRenderer renderer = renderers.get(i);
            try {
                return renderer.derefPage(page, link);
            } catch (InvalidDeref _) {
            }
        }
        
        return null;
    }

    @Override
    public synchronized void addRenderer(ObjectRenderer render)
    {
        renderers.add(render);
    }
    
    @Override
    public synchronized void removeRootPage(RootPage page)
    {
        if(rootPages.get(page.rootPageName()) == page)
            rootPages.remove(page.rootPageName());
    }

    @Override
    public synchronized void addRootPage(RootPage page)
    {
        rootPages.put(page.rootPageName(), page);
    }

    @Override
    public Collection<RootPage> rootPages()
    {
        return rootPages.values();
    }

    @Override
    public synchronized void addRootPages(Collection<RootPage> pages)
    {
        for (RootPage page : pages)
            addRootPage(page);
    }

    @Override
    public Context context()
    {
        return new DefaultContext(this, indexPage);
    }

    private void renderError(
            Writer out,
            String url,
            String desc,
            Object... args) throws IOException
    {
        out.write("<html><head><title>Error</title></head>");
        out.write(String.format("<body><h1>Error with %s</h1>",
                StringEscapeUtils.escapeHtml(url)));
        out.write(String.format("%s",
                StringEscapeUtils.escapeHtml(String.format(desc, args))));
        out.write("</body></html>");
    }

    @Override
    public synchronized void renderURL(String url, Writer writer)
            throws IOException
    {
        Output out = new Output(this, writer);
        out.html();
        out.body();

        if (url.startsWith("/")) {
            url = url.substring(1);
        }

        if (url.equals("")) {
            renderObjectDetails(out, new BaseLink(indexPage), indexPage);
        } else {
            String[] names = url.split("/");

            // Try to dereference the URL pages. This is kind of
            // grungy, but the idea is to retain the last valid
            // object we found ("object") and the last index we tried ("i").
            int i = 0;
            Object result = rootPages.get(names[0]);
            if (result != null) {
                i = 1;
                while (result != null && i < names.length) {
                    // Lookup index i:
                    Object nextObject = derefPage(result, names[i]);

                    // Index i is invalid: Nothing with that name.
                    if (nextObject == null) {
                        break;
                    }

                    // Index i is valid, store it in result and increment "i".
                    result = nextObject;
                    i++;
                }
            }

            // Successfully deref'd all pages:
            if (result == null) {
                // Completely bogus URL.
                renderError(writer, url, "No root page %s, display index",
                        names[0]);
                renderObjectDetails(out, new BaseLink(indexPage), indexPage);
            } else {
                if (i != names.length) {
                    renderError(
                            writer,
                            url,
                            "Failed to dereference #%d (%s), display last valid object.",
                            i, names[i]);
                }
                renderObjectDetails(out, new BaseLink(names, i), result);
            }
        }

        out._body();
        out._html();
    }

    @Override
    public RootPage indexPage()
    {
        return indexPage;
    }
    
}
