package com.smallcultfollowing.lathos;

import java.io.IOException;

/**
 * An object renderer that uses reflection to print a summary and other
 * details, as well as to dereference fields. 
 * 
 * @see Lathos
 */
public class ReflectiveRenderer
    implements ObjectRenderer
{
    public static class ReflectivePage
    implements Page.Detailed
    {
        private final Object obj;

        public ReflectivePage(Object obj) {
            this.obj = obj;
        }

        @Override
        public void renderObjectTitle(Output out, Link link) throws IOException {
            Lathos.reflectiveRenderTitle(obj, out, link);
        }

        @Override
        public void renderSummary(Output out, Link link) throws IOException
        {
            Lathos.reflectiveRenderSummary(obj, out, link);
        }

        @Override
        public void renderDetails(Output out, Link link) throws IOException
        {
            Lathos.reflectiveRenderDetails(obj, out, link);
        }

        @Override
        public Object derefPage(LathosServer server, String link) throws InvalidDeref
        {
            return Lathos.reflectiveDerefPage(obj, link);
        }

    }

    @Override
    public Page asPage(LathosServer server, Object obj) {
        return new ReflectivePage(obj);
    }
}
