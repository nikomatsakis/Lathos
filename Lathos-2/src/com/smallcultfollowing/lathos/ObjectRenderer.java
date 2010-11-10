package com.smallcultfollowing.lathos;

import java.io.IOException;

/** Customizes how an object is rendered to HTML */
public interface ObjectRenderer
{
    /** Renders {@code obj} onto {@code out}, returning {@code true}
     *  if rendering was successful or {@code false} if {@code obj}
     *  is not of a recognized class or is not renderable by this 
     *  renderer. */ 
    public boolean renderObject(Output out, Link link, Object obj)
    throws IOException;

    /** @see #renderObject(Output, Link, Object) */
    public boolean renderObjectAsPage(Output out, Link link, Object obj)
    throws IOException;
}
