package com.smallcultfollowing.lathos;

import java.io.IOException;

public class ThrowableRenderer
    implements ObjectRenderer
{
    public static class ThrowablePage
        extends ReflectiveRenderer.ReflectivePage
    {
        private final Throwable thr;

        public ThrowablePage(Throwable thr) {
            super(thr);
            this.thr = thr;
        }

        @Override
        public void renderDetails(Output out, Link link)
                throws IOException
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

    }

    @Override
    public Page asPage(LathosServer server, Object obj) {
        if(obj instanceof Throwable)
            return new ThrowablePage((Throwable)obj);
        return null;
    }
}
