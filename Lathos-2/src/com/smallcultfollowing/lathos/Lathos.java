package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.lang.reflect.Field;

public abstract class Lathos
{
    private static ThreadLocal<Context> currentContext = new ThreadLocal<Context>();

    /**
     * Starts and returns a "batteries included" Lathos HTTP server on port
     * {@code port}, using the default web server (currently Jetty). If port is
     * 0, returns a {@code DevNullServer}.
     */
    public static LathosServer serverOnPort(int port) throws Exception
    {
        LathosServer server = JettyServer.start(port);
        setupServer(server);
        return server;
    }

    /**
     * Performs the standard setup for a server, adding default object renders,
     * a link cache, and other goodies.
     */
    public static void setupServer(LathosServer server)
    {
        server.addRootPage(new IndexPage());
        server.addRootPage(new StaticPage());
        
        server.setLinkCache(new DefaultLinkCache(10000));
        
        server.addRenderer(new ReflectiveRenderer());
        server.addRenderer(new ConstantRenderer());
        server.addRenderer(new ThrowableRenderer());
        server.addRenderer(new CollectionRenderer());
        server.addRenderer(new PageRenderer());
    }

    public static Context context()
    {
        Context ctx = currentContext.get();
        if (ctx == null)
            return DevNullContext.instance;
        return ctx;
    }

    /**
     * Sets the current context that will be returned by context() for this
     * thread, returning the current value.
     * 
     * This method is intended to be used in a lexically-scoped fashion, like
     * so:
     * 
     * <pre>
     *  final Context oldContext = Lathos.setContext(foo);
     *  try {
     *     ...
     *  } finally {
     *     Lathos.setContext(oldContext);
     *  }
     * </pre>
     * 
     * @param newCtx
     *            the new context to use for this thread
     * @returns the current context for this thread
     */
    public static Context setContext(Context newCtx)
    {
        Context oldCtx = currentContext.get();
        currentContext.set(newCtx);
        return oldCtx;
    }

    /**
     * Creates a new context associated with {@code server} whose stack contains
     * the index page for {@code server} (if any).
     * 
     * @param server
     *            the server to associate with the context. If null, the
     *            function returns {@link DevNullContext#instance}.
     */
    public static Context newContextWithIndex(LathosServer server)
    {
        if (server == null)
            return DevNullContext.instance;

        Context ctx = server.context();

        Object indexPage = server.getIndexPage();
        if (indexPage != null && indexPage instanceof ExtensiblePage) {
            ctx.push((ExtensiblePage) indexPage);
        }

        return ctx;
    }

    /**
     * Creates a new context associated with {@code server} whose stack contains
     * the page {@code page}.
     * 
     * @param server
     *            the server to associate with the context. If null, the
     *            function returns {@link DevNullContext#instance}.
     * 
     * @param page
     *            the page to push. If null, no page is pushed.
     */
    public static Context newContextWithPage(LathosServer server, ExtensiblePage page)
    {
        if (server == null)
            return DevNullContext.instance;

        Context ctx = server.context();

        if (page != null)
            ctx.push(page);

        return ctx;
    }

    public static Line log(Object... objs)
    {
        return context().log(objs);
    }

    /**
     * Emits a table containing all fields of {@code page} and their values.
     * Fields annotated with {@link Ignore} will not be printed. This is the
     * method which is used to render an object as a page by default, unless the
     * object implements the interface {@link Page}.
     */
    public static void reflectiveRenderDetails(Object page, Output out, Link link) throws IOException
    {
        out.table();

        out.tr();
        out.th().text("Field")._th();
        out.th().text("Value")._th();
        out._tr();

        Class<?> cls = page.getClass();
        while (cls != Object.class) {
            for (Field fld : cls.getDeclaredFields()) {
                if (fld.getAnnotation(Ignore.class) != null)
                    continue;

                fld.setAccessible(true);
                out.tr();

                Link fldLink = new RelativeLink(link, fld.getName());

                out.td();
                out.text(fld.getName());
                out._td();

                out.td();
                try {
                    Object value = fld.get(page);
                    out.obj(fldLink, value);
                } catch (Exception e) {
                    out.text("Failed to access: ");
                    out.obj(fldLink, e);
                }
                out._td();

                out._tr();
            }
            cls = cls.getSuperclass();
        }

        out._table();

    }

    /**
     * Reflectively dereferences a link from this object by looking for a field
     * with that name. This is the method which is used to deref an object by
     * default, unless the object implements the interface {@link Page}.
     * 
     * Returns {@link Lathos#invalidDeref}
     * 
     * @throws InvalidDeref
     *             if there is no field {@code link}
     */
    public static Object reflectiveDerefPage(Object parentPage, String link) throws InvalidDeref
    {
        Class<?> cls = parentPage.getClass();
        while (cls != Object.class) {
            Field fld;
            try {
                fld = cls.getDeclaredField(link);
                fld.setAccessible(true);
                try {
                    return fld.get(parentPage);
                } catch (Exception e) {
                    return e;
                }
            } catch (SecurityException e1) {
            } catch (NoSuchFieldException e1) {
            }
            cls = cls.getSuperclass();
        }
        throw InvalidDeref.instance;
    }

    public static void headerRow(Output out, Object... columns) throws IOException
    {
        out.tr();
        for (Object column : columns) {
            out.th();
            out.obj(column);
            out._th();
        }
        out._tr();
    }

    public static void row(Output out, Object... columns) throws IOException
    {
        out.tr();
        for (Object column : columns) {
            out.td();
            out.obj(column);
            out._td();
        }
        out._tr();
    }

    /**
     * Reflectively renders an object as a link by by using its
     * {@link Object#toString()} value. This is the method which is used to
     * render an object by default, unless the object implements the interface
     * {@link Page}.
     */
    public static void reflectiveRenderSummary(Object obj, Output out, Link link) throws IOException
    {
        out.a(link);
        out.text(obj.toString());
        out._a(link);
    }

    /**
     * Pushes and returns a sub-page with the content {@code objs} on the
     * current context.
     */
    public static ExtensiblePage indent(Object... objs)
    {
        Context ctx = Lathos.context();
        ExtensiblePage page = ctx.newPage(null);
        ctx.embed(page);
        ctx.push(page);
        ctx.log(objs);
        return page;
    }
}
