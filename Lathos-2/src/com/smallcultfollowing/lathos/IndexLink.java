package com.smallcultfollowing.lathos;

public class IndexLink
    implements Link
{
    private final Link base;
    private final int index;
    
    public IndexLink(Link base, int index)
    {
        super();
        this.base = base;
        this.index = index;
    }

    @Override
    public void appendUrlString(StringBuilder sb)
    {
        base.appendUrlString(sb);
        sb.append("/");
        sb.append(index);
    }

    @Override
    public boolean isValid()
    {
        return base != null && base.isValid();
    }

    public static int parseIndexLink(String link) throws InvalidDeref {
        try {
            int idx = Integer.parseInt(link);
            if (idx < 0) throw InvalidDeref.instance;
            return idx;
        } catch (NumberFormatException e) {
            throw InvalidDeref.instance;
        }
    }
    
}
