package com.smallcultfollowing.lathos;

import java.io.IOException;

/** 
 * An object renderer which renders strings and numbers as themselves,
 * without creating any kind of link to them.
 */
public class ConstantRenderer
    implements ObjectRenderer
{

    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link) throws IOException
    {
        if(obj instanceof String || obj instanceof Number) {
            out.text(obj.toString());
            return true;
        }
        return false;
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link) throws IOException
    {
        return renderObjectSummary(obj, out, link);
    }

    @Override
    public Object derefPage(Object obj, String link) throws InvalidDeref
    {
        throw InvalidDeref.instance;
    }

}
