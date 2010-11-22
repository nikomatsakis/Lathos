package com.smallcultfollowing.lathos;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * An annotation that can be attached to methods to allow Lathos reflection to
 * invoke them.
 * 
 * @see Ignore
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface AllowReflectiveDeref
{
    /**
     * If true, then this method will be listed by
     * {@link Lathos#reflectiveRenderDetails(Object, Output, Link)} amongst the
     * fields.
     */
    public boolean showInDetails() default true;
}
