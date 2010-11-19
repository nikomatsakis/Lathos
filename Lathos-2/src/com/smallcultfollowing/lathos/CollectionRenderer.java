package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;

public class CollectionRenderer
    implements ObjectRenderer
{

    @Override
    public boolean renderObjectSummary(Object obj, Output out, Link link) throws IOException
    {
        if (obj instanceof Object[])
            obj = Arrays.asList((Object[]) obj);

        if (obj instanceof Iterable) {
            Iterable<?> iter = (Iterable<?>) obj;
            renderIterableSummary(iter, out, link);
            return true;
        }

        if (obj instanceof Map.Entry) {
            Map.Entry<?, ?> entry = (Entry<?, ?>) obj;
            renderEntrySummary(entry, out);
            return true;
        }

        if (obj instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) obj;
            renderIterableSummary(map.entrySet(), out, link);
            return true;
        }

        return false;
    }

    public static void renderEntrySummary(Map.Entry<?, ?> entry, Output out) throws IOException
    {
        // Note: in general, links to Map.Entries come through maps, which cannot
        // be easily dereferenced.  Therefore, we synthesize links to the items
        // using the link cache:
        out.text("(");
        out.obj(entry.getKey());
        out.code().text(" -> ")._code();
        out.obj(entry.getValue());
        out.text(")");
    }

    public static void renderIterableSummary(Iterable<?> iter, Output out, Link link) throws IOException
    {
        out.text("[");

        Iterator<?> elems = iter.iterator();
        for (int i = 0; i < 3 && elems.hasNext(); i++) {
            Object elem = elems.next();
            out.obj(new IndexLink(link, i), elem);
            if (elems.hasNext())
                out.text(", ");
        }

        if (elems.hasNext()) {
            out.a(link);
            out.text("...");
            out._a(link);
        }

        out.text("]");
    }

    @Override
    public boolean renderObjectDetails(Object obj, Output out, Link link) throws IOException
    {
        if (obj instanceof Object[])
            obj = Arrays.asList((Object[]) obj);

        if (obj instanceof Iterable) {
            Iterable<?> lst = (Iterable<?>) obj;
            renderIterableDetails(lst, out, link);
            return true;
        }
        
        if (obj instanceof Map) {
            Map<?, ?> map = (Map<?, ?>) obj;
            renderMapDetails(map, out, link);
            return true;
        }

        return false;
    }

    public static void renderMapDetails(Map<?, ?> map, Output out, Link link) throws IOException
    {
        out.table();
        for(Map.Entry<?, ?> entry : map.entrySet()) {
            Lathos.row(out, entry.getKey(), entry.getValue());
        }
        out._table();
    }

    public static void renderIterableDetails(Iterable<?> lst, Output out, Link link) throws IOException
    {
        out.ol();
        int i = 0;
        for (Object elem : lst) {
            out.li();
            out.obj(new IndexLink(link, i++), elem);
            out._li();
        }
        out._ol();
    }

    @Override
    public Object derefPage(Object obj, String link) throws InvalidDeref
    {
        if (obj instanceof List) {
            int idx = IndexLink.parseIndexLink(link);
            return ((List<?>) obj).get(idx);
        } 
        
        if (obj instanceof Object[]) {
            int idx = IndexLink.parseIndexLink(link);
            return ((Object[]) obj)[idx];
        } 
        
//        if (obj instanceof Map.Entry) {
//            Map.Entry<?,?> entry = (Entry<?,?>) obj;
//            if(link.equals("key")) return entry.getKey();
//            if(link.equals("value")) return entry.getValue();
//        }
//        
//        if (obj instanceof Map) {
//            // Try using link as a key:
//            Map<?, ?> map = (Map<?, ?>) obj;
//            // Object value = map.get(link);
//            // if(value != null)
//            //    return value;
//                
//            // Not a key, use the index in the entry set:
//            // (Requires that entry set indices remain stable...)
//            obj = map.entrySet();
//        }
        
        if (obj instanceof Iterable) {
            int idx = IndexLink.parseIndexLink(link);
            Iterator<?> iter = ((Iterable<?>) obj).iterator();
            while (iter.hasNext()) {
                Object elem = iter.next();
                if (idx-- == 0)
                    return elem;
            }
            throw InvalidDeref.instance;
        }

        throw InvalidDeref.instance;
    }

}
