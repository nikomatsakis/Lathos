package com.smallcultfollowing.lathos;

import java.io.IOException;

public interface Page 
extends Debugable
{
    public void renderAsPage(Output out, Link link) throws IOException;
    
    /** Dereferences a relative link to yield the next
     *  step.  For example, if there is a link "/a/b/c", and
     *  "/a/b" leads to {@code this}, then this method would be invoked
     *  with "c" to yield the next step.  If the link were
     *  "/a/b/c/d" instead, then the result of this method would
     *  then itself be casted to {@link Page} and have
     *  {@link #derefPage(String)} invoked with "d" as argument. */
    public Object derefPage(String link);
}
