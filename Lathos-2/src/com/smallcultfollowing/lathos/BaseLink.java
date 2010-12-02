package com.smallcultfollowing.lathos;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.net.URLEncoder;
import java.util.Arrays;

public class BaseLink
    implements Link
{
    private final String[] names;

    public BaseLink(RootPage rootPage)
    {
        this(rootPage.rootPageName());
    }

    public BaseLink(String name)
    {
        super();
        this.names = new String[] { name };
    }

    public BaseLink(String[] names, int length)
    {
        super();
        this.names = Arrays.copyOf(names, length);
    }

    @Override
    public void appendUrlString(StringBuilder sb)
    {
        sb.append("/");
        for (int i = 0; i < names.length - 1; i++) {
            sb.append(encode(names[i]));
            sb.append("/");
        }
        sb.append(encode(names[names.length - 1]));
    }

    @Override
    public boolean isValid()
    {
        return true;
    }

    @Override
    public String toString()
    {
        return toString(this);
    }

    public static String encode(String name)
    {
        try {
            return URLEncoder.encode(name, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    public static String decode(String url)
    {
        try {
            return URLDecoder.decode(url, "UTF-8");
        } catch (UnsupportedEncodingException e) {
            throw new RuntimeException(e);
        }
    }

    public static String toString(Link link)
    {
        StringBuilder sb = new StringBuilder();
        link.appendUrlString(sb);
        return sb.toString();
    }

    public static String[] decodeIntoNames(String url)
    {
        if (url.startsWith("/"))
            url = url.substring(1);
        String[] names = url.split("/");
        for (int i = 0; i < names.length; i++) {
            names[i] = BaseLink.decode(names[i]);
        }
        return names;
    }

}
