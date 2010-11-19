package com.smallcultfollowing.lathos;

import java.io.IOException;

public class ThrowableRenderer
    implements ObjectRenderer
{
    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link)
            throws IOException
    {
        if (obj instanceof Throwable) {
            out.a(link).text("<Throwable: "+obj+">");
            out._a(link);
        }
        return false;
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link)
            throws IOException
    {
        if(obj instanceof Throwable) {
            renderDetails(out, link, (Throwable)obj);
            return true;
        }
        return false;
    }

    private void renderDetails(Output out, Link link, Throwable thr) throws IOException
    {
        Throwable t = thr;
        out.ul();
        while(t != null) {
            if(t != thr) {
                out.li();
                out.text("Caused by:");
                out.ul();
            }
            
            out.li().b().text("Class: ")._b().text(t.getClass().getName())._li();
            out.li().b().text("Message: ")._b().text(t.getMessage())._li();
            
            out.li().b().text("Stack:")._b().ul();
            for(StackTraceElement elem : t.getStackTrace()) {
                out.li();
                out.obj(null, elem);
                out._li();
            }
            out._ul()._li();
            
            if(t != thr) {
                out._ul();
                out._li();
            }
            
            t = t.getCause();
        }
        out._ul();
    }

    @Override
    public Object derefPage(Object obj, String link) throws InvalidDeref
    {
        if(obj instanceof Throwable) {
            return Lathos.reflectiveDerefPage(obj, link);
        }
        throw InvalidDeref.instance;
    }

}
