package com.smallcultfollowing.lathos;

public abstract class Lathos
{
    private static ThreadLocal<Context> currentContext = new ThreadLocal<Context>();

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
    
    public static String toString(Link link) {
        StringBuilder sb = new StringBuilder();
        link.appendUrlString(sb);
        return sb.toString();
    }
}
