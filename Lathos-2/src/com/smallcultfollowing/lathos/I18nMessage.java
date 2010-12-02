package com.smallcultfollowing.lathos;

import java.io.IOException;

public class I18nMessage
    implements Page
{
    private final String messageName;
    private final Object[] arguments;

    public I18nMessage(String messageName, Object[] arguments)
    {
        super();
        this.messageName = messageName;
        this.arguments = arguments;
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.renderI18nSummary(messageName, arguments, out, link, link);
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        int i = IndexLink.parseIndexLink(link);
        if (i < arguments.length)
            return arguments[i];
        throw InvalidDeref.instance;
    }

}
