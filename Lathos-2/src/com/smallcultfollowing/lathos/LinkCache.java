package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;

public class LinkCache
    implements RootPage
{
    private final int linkRecordSize;
    private LinkRecord[] linkRecords = null;
    private long nextLinkId = 0;
    private Link baseLink = new BaseLink(this);
    
    public LinkCache(int linkRecordSize)
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
    public void renderAsLine(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderAsLine(this, out, link);
    }

    @Override
    public void renderAsPage(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderAsPage(this, out, link);
    }

    @Override
    public Object derefPage(String link)
    {
        long id = Long.parseLong(link);
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

}
