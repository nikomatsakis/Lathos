package com.smallcultfollowing.lathos;

/**
 * A container for other objects.  When rendered, simply renders its
 * various contained objects.
 */
public interface Line
    extends Page
{
    /** 
     * Adds the given set of objects to the line. 
     *  
     * <b>Warning 1:</b> since line objects are independent
     * from any particular {@link LathosServer}, no 
     * substitutions are performed on {@code objs}.
     * If you wish to have substitutions, use
     * {@link Context#append(Line, Object...)}
     * instead.
     * 
     * <b>Warning 2:</b> Takes ownership of the {@code objs}
     * array (as always when using {@code ...} arguments).
     * 
     * @see ObjectSubst
     * @see LathosServer#addSubstitutionFilter(ObjectSubst) */
    public void addObjectsToLine(Object... objs);
}
