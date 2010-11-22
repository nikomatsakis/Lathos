package com.smallcultfollowing.lathos;

public class RelativeLink
    implements Link
{
    private final Link base;
    private final String next;
    
    public RelativeLink(Link baseLink, String next)
    {
        super();
        this.base = baseLink;
        this.next = next;
    }

    @Override
    public void appendUrlString(StringBuilder sb)
    {
        base.appendUrlString(sb);
        sb.append("/");
        sb.append(BaseLink.encode(next));
    }

    @Override
    public boolean isValid()
    {
        return base != null && base.isValid();
    }
    
    @Override
    public String toString() 
    {
        return BaseLink.toString(this);
    }
}
