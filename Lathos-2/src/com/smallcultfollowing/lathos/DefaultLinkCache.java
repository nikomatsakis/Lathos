package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;

public class DefaultLinkCache
    implements LinkCache
{
    private final int linkRecordSize;
    private LinkRecord[] linkRecords = null;
    private long nextLinkId = 0;
    private Link baseLink = new BaseLink(this);
    
    public DefaultLinkCache(int linkRecordSize)
    {
        super();
        this.linkRecordSize = linkRecordSize;
    }

    class LinkRecord
    {
        long linkId;
        Reference<Object> node;

        private LinkRecord(long linkId, Object node)
        {
            super();
            this.linkId = linkId;
            this.node = new SoftReference<Object>(node);
        }
    }

    /* (non-Javadoc)
     * @see com.smallcultfollowing.lathos.LinkCacheInterface#makeLink(java.lang.Object)
     */
    @Override
    public synchronized Link makeLink(Object node)
    {
        if (linkRecords == null) {
            linkRecords = new LinkRecord[linkRecordSize];
        }

        long id = nextLinkId++;
        int index = (int) (id % linkRecords.length);
        linkRecords[index] = new LinkRecord(id, node);
        return new RelativeLink(baseLink, Long.toString(id));
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderDetails(this, out, link);
    }

    @Override
    public Object derefPage(String link) throws InvalidDeref
    {
        long id;
        try {
            id = Long.parseLong(link);
        } catch (NumberFormatException e) {
            return Lathos.reflectiveDerefPage(this, link);
        }
        
        if(linkRecords != null) {
            int index = (int)(id % linkRecords.length);
            LinkRecord lr = linkRecords[index];
            if(lr != null) {
                if (lr.linkId == id) {
                    Object node = lr.node.get();
                    if(node != null)
                        return node;
                }
            }
        }
        return "Stale or invalid link";
    }

    @Override
    public String rootPageName()
    {
        return "link-cache";
    }
    
    @Override
    public String toString()
    {
        return rootPageName();
    }

}
