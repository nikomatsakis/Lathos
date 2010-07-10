package com.smallcultfollowing.lathos;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Used to annotate the field storing the subcontent of
 * a custom page.  {@link Lathos#reflectivePageContents(Page, Output)}
 * will iterate through the contents of such a field, invoking
 * {@link PageContent#renderInPage(Output)} on each member.
 * 
 * This annotation must only be applied to fields of type
 * {@link Iterable} which contain {@link PageContent} members. 
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.FIELD)
public @interface PageSubcontent {

}
