package com.smallcultfollowing.lathos;

import java.util.Arrays;


public class BaseLink
    implements Link
{
    private final String[] names;

    public BaseLink(RootPage rootPage)
    {
        super();
        this.names = new String[] { rootPage.rootPageName() };
    }
    
    public BaseLink(String[] names, int length) {
        super();
        this.names = Arrays.copyOf(names, length);
    }
    
    @Override
    public void appendUrlString(StringBuilder sb)
    {
        sb.append("/");
        for(int i = 0; i < names.length - 1; i++) {
            sb.append(names[i]);
            sb.append("/");
        }
        sb.append(names[names.length - 1]);
    }

    @Override
    public boolean isValid()
    {
        return true;
    }

}
