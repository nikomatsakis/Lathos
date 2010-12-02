package com.smallcultfollowing.lathos.test;

public class ConsPair
{
    public final String value;
    public final ConsPair next;

    public ConsPair(String value, ConsPair next)
    {
        super();
        this.value = value;
        this.next = next;
    }

    @Override
    public String toString()
    {
        return String.format("[%s, %s]", value, next);
    }
}
