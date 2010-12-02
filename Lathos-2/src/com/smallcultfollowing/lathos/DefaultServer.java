package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.ResourceBundle;

import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public abstract class DefaultServer
    extends HttpServlet
    implements LathosServer
{
    private volatile ObjectSubst[] substs = new ObjectSubst[0];
    private final List<ObjectRenderer> renderers = new ArrayList<ObjectRenderer>();
    private final Map<String, Object> rootPages = new LinkedHashMap<String, Object>();
    private ResourceBundle resourceBundle = null;
    private LinkCache linkCache = null;
    private int maxEmbedDepth = 100;
    private LathosServerDelegate delegate = new DefaultDelegate();

    /** Equivalent to {@code this(true, 10000)} */
    DefaultServer()
    {
    }

    @Override
    public synchronized void setLinkCache(LinkCache aLinkCache)
    {
        if (linkCache != null) {
            removeRootPage(linkCache);
        }

        linkCache = aLinkCache;

        if (linkCache != null) {
            addRootPage(linkCache);
        }
    }

    @Override
    public synchronized Link defaultLink(Object obj)
    {
        if (linkCache == null)
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
    public Page asPage(Object obj)
    {
        for (int i = renderers.size() - 1; i >= 0; i--) {
            ObjectRenderer renderer = renderers.get(i);
            Page page = renderer.asPage(this, obj);
            if (page != null)
                return page;
        }

        return new ConstantRenderer.ConstantPage(obj);
    }

    private Object derefPage(Object obj, String link)
    {
        Page page = asPage(obj);
        try {
            return page.derefPage(this, link);
        } catch (InvalidDeref _) {
            return null;
        }
    }

    @Override
    public synchronized void addRenderer(ObjectRenderer render)
    {
        renderers.add(render);
    }

    @Override
    public synchronized void removeRootPage(RootPage page)
    {
        if (rootPages.get(page.rootPageName()) == page)
            rootPages.remove(page.rootPageName());
    }

    @Override
    public synchronized void addRootPage(String link, Object page)
    {
        rootPages.put(link, page);
    }

    @Override
    public synchronized void addRootPage(RootPage page)
    {
        addRootPage(page.rootPageName(), page);
    }

    @Override
    public Map<String, Object> rootPages()
    {
        return Collections.unmodifiableMap(rootPages);
    }

    @Override
    public synchronized void addRootPages(Collection<RootPage> pages)
    {
        for (RootPage page : pages)
            addRootPage(page);
    }

    @Override
    public synchronized Context context()
    {
        return new DefaultContext(this);
    }

    @Override
    protected synchronized void doGet(HttpServletRequest req, HttpServletResponse resp) throws ServletException,
            IOException
    {
        String url = req.getRequestURI();
        if (url.equals("/")) {
            Link link = new BaseLink(LathosServer.indexName);
            resp.sendRedirect(link.toString());
            return;
        }

        renderURL(req, resp);
    }

    public synchronized void renderURL(HttpServletRequest req, HttpServletResponse resp)
            throws IOException
    {
        String url = req.getRequestURI();
        PrintWriter writer = resp.getWriter();
        try {
            Output out = new Output(this, req, resp, writer);
            
            if(delegate.handleRequest(url, out))
                return;
    
            String[] names = BaseLink.decodeIntoNames(url);
    
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
    
            // Render the page we found, or the last valid one, following back to the index if nothing else:
            if (result == null) {
                renderRootPage(out, new BaseLink(indexName), getIndexPage());
            } else {
                renderRootPage(out, new BaseLink(names, i), result);
            }
        } finally {
            writer.close();
        }
    }

    private void renderRootPage(Output out, BaseLink link, Object rootPage) throws IOException
    {
        // Hokey: special case non-html content.
        if (rootPage instanceof NonHtmlPage) {
            ((NonHtmlPage) rootPage).renderNonHtml(out.request, out.response, link);
        } else {
            delegate.startHtmlPage(out, link, rootPage);
            out.embed(link, rootPage);
            delegate.endHtmlPage(out, link, rootPage);
        }
    }

    @Override
    public synchronized Object getIndexPage()
    {
        return rootPages.get(indexName);
    }

    @Override
    public synchronized void setResourceBundle(ResourceBundle resourceBundle)
    {
        this.resourceBundle = resourceBundle;
    }

    @Override
    public synchronized ResourceBundle getResourceBundle()
    {
        return resourceBundle;
    }

    @Override
    public synchronized void setMaxEmbedDepth(int maxDepth)
    {
        this.maxEmbedDepth = maxDepth;
    }

    @Override
    public synchronized int getMaxEmbedDepth()
    {
        return maxEmbedDepth;
    }

    @Override
    public LathosServerDelegate getDelegate()
    {
        return delegate;
    }

    @Override
    public synchronized void setDelegate(LathosServerDelegate delegate)
    {
        assert delegate != null;
        this.delegate = delegate;
    }

}
