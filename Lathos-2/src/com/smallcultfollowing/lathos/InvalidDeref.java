package com.smallcultfollowing.lathos;

/**
 * Thrown by {@link ObjectRenderer#derefPage(Object, LathosServer, String)}
 * instances when the link cannot be resolved. It is best to use the
 * pre-constructed {@link #instance} for efficiency reasons.
 */
public class InvalidDeref
    extends Exception
{
    public static final InvalidDeref instance = new InvalidDeref();
}
