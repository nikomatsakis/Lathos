package com.smallcultfollowing.lathos;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Fields that bear this annotation are ignored by
 * {@link Lathos#reflectiveFields(Object, Output).
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface Ignore
{

}
