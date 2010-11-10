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

public class DefaultServer
    implements LathosServer
{
    private volatile ObjectSubst[] substs = null;
    private volatile int substCount = 0;

    private final List<ObjectRenderer> renderers = new ArrayList<ObjectRenderer>();
    private final Map<String, RootPage> rootPages = new LinkedHashMap<String, RootPage>();
    private final IndexPage indexPage = new IndexPage();
    
    DefaultServer() 
    {
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
        int substCount = this.substCount;
        ObjectSubst[] substs = this.substs;

        for (int i = 0; i < substCount; i++) {
            obj = substs[i].substitute(obj);
        }

        return obj;
    }

    @Override
    public synchronized void addSubstitutionFilter(ObjectSubst subst)
    {
        if (substCount + 1 < substs.length) {
            substs[substCount] = subst;
        } else {
            ObjectSubst[] newSubsts = Arrays.copyOf(substs, substs.length * 2);
            newSubsts[substs.length] = subst;

            // Important: fully initialize newSubsts before "publishing" it.
            substs = newSubsts;
        }

        // Note: it is important that we write substCount *after* writing to
        // substs and its contents.
        substCount = substCount + 1;
    }

    @Override
    public synchronized void renderObject(Output out, Link link, Object obj) throws IOException
    {
        for(int i = renderers.size() - 1; i >= 0; i--) {
            ObjectRenderer renderer = renderers.get(i);
            if (renderer.renderObject(out, link, obj))
                return;
        }

        if(obj instanceof Debugable) {
            ((Debugable) obj).renderAsLine(out, link);
            return;
        }

        out.text(obj.toString());
    }

    @Override
    public synchronized void addRenderer(ObjectRenderer render)
    {
        renderers.add(render);
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
        
        if (url.equals("")) {
            renderObjectAsPage(out, new BaseLink(indexPage), indexPage);
        } else {
            String[] names = url.split("/");

            // Try to dereference the URL pages.  This is kind of
            // grungy, but the idea is to retain the last valid
            // object we found ("object") and the last index we tried ("i").
            int i = 0;
            Object result = rootPages.get(names[0]);
            while(result != null && i < names.length) {
                // Index i is invalid, previous index cannot be deref: 
                if(!(result instanceof Page)) {
                    break;
                }
                
                // Lookup index i:
                Page resultPage = (Page) result;
                Object nextObject = resultPage.derefPage(names[i]);
                
                // Index i is invalid: Nothing with that name.
                if(nextObject == null) {
                    break;
                }
                
                // Index i is valid, store it in result and increment "i".
                result = nextObject;
                i++;
            }

            // Successfully deref'd all pages:
            if(result == null) {
                // Completely bogus URL.
                renderError(writer, url, "No root page %s, display index", names[0]);
                renderObjectAsPage(out, new BaseLink(indexPage), indexPage);
            } else {
                if (i != names.length) {
                    renderError(writer, url, "Failed to dereference #%d (%s), display last valid object.", i, names[i]);
                }
                renderObjectAsPage(out, new BaseLink(names, i), result);
            }
        }
    
        out._body();
        out._html();
    }

    @Override
    public void renderObjectAsPage(Output out, Link link, Object obj) throws IOException
    {
        if(obj instanceof Page) {
            Page page = (Page) obj;
            page.renderAsPage(out, link);
        } else {
            renderObject(out, link, obj);
        }
    }

    @Override
    public RootPage indexPage()
    {
        return indexPage;
    }

}
