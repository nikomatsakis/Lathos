package com.smallcultfollowing.lathos;

import java.io.IOException;

/** 
 * An object renderer which renders strings and numbers as themselves,
 * without creating any kind of link to them.
 */
public class ConstantRenderer
    implements ObjectRenderer
{
    public static class ConstantPage
        implements Page
    {
        private final Object constant;

        public ConstantPage(Object constant) {
            this.constant = constant;
        }

        @Override
        public void renderSummary(Output out, Link link) throws IOException {
            out.text(constant.toString());
        }

        @Override
        public Object derefPage(LathosServer server, String link) throws InvalidDeref {
            throw InvalidDeref.instance;
        }
    }

    @Override
    public Page asPage(LathosServer server, Object obj) {
        if(obj instanceof String || obj instanceof Number) {
            return new ConstantPage(obj);
        }
        return null;
    }

}
