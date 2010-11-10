package com.smallcultfollowing.lathos;

import java.io.IOException;

public class ThrowableRenderer
    implements ObjectRenderer
{
    @Override
    public boolean renderObject(Output out, Link link, Object obj)
            throws IOException
    {
        if (obj instanceof Throwable) {
            out.a(link).text("<Throwable: "+obj+">");
            out._a(link);
        }
        return false;
    }

    @Override
    public boolean renderObjectAsPage(Output out, Link link, Object obj)
            throws IOException
    {
        if(obj instanceof Throwable) {
            renderAsPage(out, link, (Throwable)obj);
            return true;
        }
        return false;
    }

    private void renderAsPage(Output out, Link link, Throwable thr) throws IOException
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
                out.renderObject(null, elem);
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

}
