package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

public class CollectionRenderer
    implements ObjectRenderer
{

    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link) throws IOException
    {
        if(obj instanceof Object[])
            obj = Arrays.asList((Object[])obj);
        
        if(obj instanceof List) {
            List<?> lst = (List<?>) obj;
            out.text("[");
            for(int i = 0; i < Math.min(lst.size(), 3); i++) {
                if(i > 0) out.text(", ");
                out.obj(new IndexLink(link, i), lst.get(i));
            }
            
            if(lst.size() > 3) {
                out.a(link);
                out.text(", ...");
                out._a(link);
            }
            
            out.text("]");
            return true;
        }

        if(obj instanceof Iterable) {
            
        }

        return false;
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link) throws IOException
    {
        if(obj instanceof Object[])
            obj = Arrays.asList((Object[])obj);
        
        if(obj instanceof List) {
            List<?> lst = (List<?>) obj;
            out.ol();
            for(int i = 0; i < lst.size(); i++) {
                out.li();
                out.obj(new IndexLink(link, i), lst.get(i));
                out._li();
            }
            out._ol();
            return true;
        }

        if(obj instanceof Iterable) {
            
        }
        
        return false;
    }

    @Override
    public Object derefPage(Object obj, String link) throws InvalidDeref
    {
        if(obj instanceof List) {
            int idx = Integer.parseInt(link);
            return ((List<?>) obj).get(idx);
        } else if (obj instanceof Object[]) {
            int idx = Integer.parseInt(link);
            return ((Object[])obj)[idx];
        } else if (obj instanceof Map) {
            Object result = ((Map<?,?>) obj).get(link);
            if (result != null) return result;
        }
        
        throw InvalidDeref.instance;
    }

}
