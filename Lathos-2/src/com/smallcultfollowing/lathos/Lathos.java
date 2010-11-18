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
        server.addRootPage(new StaticPage());
        server.setLinkCache(new LinkCache(10000));
        return server;
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
    public static void reflectiveRenderAsPage(Object page, Output out, Link link) throws IOException
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
                    out.renderObject(fldLink, value);
                } catch (Exception e) {
                    out.text("Failed to access: ");
                    out.renderObject(fldLink, e);
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
     */
    public static Object reflectiveDerefPage(Object parentPage, String link)
    {
        Class<?> cls = parentPage.getClass();
        while (cls != Object.class) {
            for (java.lang.reflect.Field fld : cls.getFields()) {
                if (fld.getName().equals(link)) {
                    try {
                        return fld.get(parentPage);
                    } catch (Exception e) {
                        return e;
                    }
                }
            }
            cls = cls.getSuperclass();
        }
        return null;
    }

    public static void headerRow(Output out, Object... columns) throws IOException
    {
        out.tr();
        for (Object column : columns) {
            out.th();
            out.renderObject(null, column);
            out._th();
        }
        out._tr();
    }

    public static void row(Output out, Object... columns) throws IOException
    {
        out.tr();
        for (Object column : columns) {
            out.td();
            out.renderObject(null, column);
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
    public static void reflectiveRenderAsLine(Object obj, Output out, Link link) throws IOException
    {
        out.a(link);
        out.text(obj.toString());
        out._a(link);
    }
}
