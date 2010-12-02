package com.smallcultfollowing.lathos.test;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import com.smallcultfollowing.lathos.InvalidDeref;
import com.smallcultfollowing.lathos.Lathos;
import com.smallcultfollowing.lathos.LathosServer;
import com.smallcultfollowing.lathos.Link;
import com.smallcultfollowing.lathos.Output;
import com.smallcultfollowing.lathos.Page;
import com.smallcultfollowing.lathos.RelativeLink;

public class UrlEscape
implements Page.Detailed
{
    public final Map<String, String> map = new HashMap<String, String>();
    
    public UrlEscape() {
        // some characters that can't (normally) go into URLs:
        map.put("→", "right arrow");
        map.put("←", "left arrow");
        map.put("α", "alpha");
        map.put("foo/bar(&test +why)", "random stuff");
    }

    @Override
    public void renderSummary(Output out, Link link) throws IOException
    {
        Lathos.reflectiveRenderSummary(this, out, link);
    }

    @Override
    public void renderObjectTitle(Output out, Link link) throws IOException {
        Lathos.reflectiveRenderTitle(this, out, link);
    }

    @Override
    public void renderDetails(Output out, Link link) throws IOException
    {
        out.h3().text("URL Escaping Test")._h3();
        
        out.ul();
        for(Map.Entry<String, String> entry : map.entrySet()) {
            out.li();
            
            Link relLink = new RelativeLink(link, entry.getKey());
            out.a(relLink);
            out.text("Key: ").text(entry.getKey()).text(" → ").text(entry.getValue());
            out._a(relLink);
            
            out._li();
        }
        out._ul();
    }

    @Override
    public Object derefPage(LathosServer server, String link) throws InvalidDeref
    {
        return Lathos.invalidDerefIfNull(map.get(link));
    }    
    
    
}
