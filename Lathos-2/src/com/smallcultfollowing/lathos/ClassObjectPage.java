package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.lang.reflect.Field;
import java.lang.reflect.Modifier;

public class ClassObjectPage
    implements Page
{
    private final Class<?> cls;
    
    public ClassObjectPage(Class<?> cls)
    {
        super();
        this.cls = cls;
    }

    @Override
    public void renderAsLine(Output out, Link link) throws IOException
    {
        out.a(link).text(cls.toString()).text("]");
        out._a(link);
    }

    @Override
    public void renderAsPage(Output out, Link link) throws IOException
    {
        out.h1().text(cls.toString())._h1();
        
        out.table();
        out.tr();
        out.th().text("Static Field")._th();
        out.th().text("Value")._th();
        out._tr();
        for(Field f : cls.getDeclaredFields()) {
            if(Modifier.isStatic(f.getModifiers())) {
                out.tr();
                String name = f.getName();
                out.td().text(name)._td();
                
                out.td();
                Link fLink = new RelativeLink(link, f.getName());
                Object value;
                try {
                    f.setAccessible(true);
                    value = f.get(null);
                } catch (IllegalArgumentException e) {
                    value = e;
                } catch (IllegalAccessException e) {
                    value = e;
                }
                out.renderObject(fLink, value);
                out._td();
                out._tr();
            }
        }
        out._table();
    }

    @Override
    public Object derefPage(String link)
    {
        try {
            Field f = cls.getDeclaredField(link);
            f.setAccessible(true);
            return f.get(null);
        } catch (SecurityException e) {
            return e;
        } catch (NoSuchFieldException e) {
            return e;
        } catch (IllegalArgumentException e) {
            return e;
        } catch (IllegalAccessException e) {
            return e;
        }
    }

}
