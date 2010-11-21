package com.smallcultfollowing.lathos;

import java.io.IOException;
import java.text.AttributedCharacterIterator;
import java.text.MessageFormat;
import java.util.ResourceBundle;

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

    private boolean indicesEqual(Integer oldIndex, Integer newIndex)
    {
        if(oldIndex == newIndex)
            return true;

        if(oldIndex == null || newIndex == null)
            return false;
        
        return newIndex.equals(oldIndex);
    }
    
    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        ResourceBundle bundle = out.server.getResourceBundle();
        if(bundle != null) {
            String messageFmt = bundle.getString(messageName);
            if(messageFmt != null) {
                MessageFormat fmt = new MessageFormat(messageFmt);
                AttributedCharacterIterator iter = fmt.formatToCharacterIterator(arguments);
                
                Integer prevArgument = null;
                Link currentLink = null;
                
                // XXX This is probably not ideal.  Maybe we should replace {1}
                // XXX with a call to argument.renderSummary(), rather than
                // XXX relying on MessageFormat to supply the toString() value.
                
                while(iter.getIndex() < iter.getEndIndex()) {
                    Integer argument = (Integer) iter.getAttribute(MessageFormat.Field.ARGUMENT);
                    
                    if (!indicesEqual(prevArgument, argument)) {
                        if(currentLink != null)
                            out._a(currentLink);
                        
                        if(argument != null) {
                            currentLink = new IndexLink(link, argument);
                            out.a(currentLink);
                        } else {
                            currentLink = null;
                        }
                        
                        prevArgument = argument;
                    }
                    
                    out.text(Character.toString(iter.next()));
                }
            }
        } else {
            out.text(messageName);
            out.text("(");
            for(int i = 0; i < arguments.length; i++) {
                out.obj(link, i, arguments[i]);
            }
            out.text(")");
        }
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        renderSummary(out, link);
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        int i = IndexLink.parseIndexLink(link);
        if(i < arguments.length)
            return arguments[i];
        throw InvalidDeref.instance;
    }

}
