package com.smallcultfollowing.lathos;

/** Filters objects before they are added to a log. */
public interface ObjectSubst
{
    /** Returns a new object to represent {@code obj}, 
     *  or {@code obj} if it cannot perform any substitution. */
    public Object substitute(Object obj);
}
