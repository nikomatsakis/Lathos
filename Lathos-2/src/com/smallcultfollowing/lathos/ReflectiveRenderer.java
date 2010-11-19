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

    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(obj, out, link);
        return true;
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderDetails(obj, out, link);
        return true;
    }

    @Override
    public Object derefPage(Object obj, String link) throws InvalidDeref
    {
        return Lathos.reflectiveDerefPage(obj, link);
    }

}
