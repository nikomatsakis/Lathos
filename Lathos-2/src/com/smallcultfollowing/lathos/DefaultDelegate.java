package com.smallcultfollowing.lathos;

import static org.rendersnake.AttributesFactory.href;
import static org.rendersnake.AttributesFactory.id;
import static org.rendersnake.AttributesFactory.onclick;

import java.io.IOException;
import java.util.LinkedList;

import org.rendersnake.AttributesFactory;

public class DefaultDelegate
    implements LathosServerDelegate
{
    private int maxId = 0;
    private final LinkedList<String> idStack = new LinkedList<String>();
    private int currentBackgroundColor = 0;
    private final String[] backgroundColors = new String[] { "E6FAFF", "B2B091", "FFFDE9", "CCA6B2", "B29AA2" };

    @Override
    public void startHtmlPage(Output out, Link link, Object rootPage) throws IOException
    {
        out.html();
        
        out.script(AttributesFactory.type("text/javascript"));
        out.println("function toggleId(id)");
        out.println("{");
        out.println("    var target = document.getElementById(id);");
        out.println("    var kids = target.childNodes;");
        out.println("    var openedKids = false;");
        out.println("    var closedKids = false;");
        out.println("    for(var i = 0; (i < kids.length); i++) {");
        out.println("        var kid = kids[i];");
        out.println("        if(");
        out.println("            kid.className == 'content' ||");
        out.println("            kid.className == 'log initiallyOpen' ||");
        out.println("            kid.className == 'log initiallyClosed'");
        out.println("        ) {");
        out.println("            if(kid.style.display == 'none') {");
        out.println("                kid.style.display = 'block';");
        out.println("                openedKids = true;");
        out.println("            } else {");
        out.println("                kid.style.display = 'none';");
        out.println("                closedKids = true;");
        out.println("            }");
        out.println("        }");
        out.println("    }");
        out.println("    ");
        out.println("    if(openedKids) {");
        out.println("        target.style.opacity = 1.0;");
        out.println("    } else if (closedKids) {");
        out.println("        target.style.opacity = 0.25;");
        out.println("    }");
        out.println("}");
        out._script();

        out.style();
        out.println("DIV.log {");
        out.println("    border-width: thin;");
        out.println("    border-style: solid;");
        out.println("    margin-top: .1cm;");
        out.println("    margin-bottom: .1cm;");
        out.println("    margin-left: .3cm;                    ");
        out.println("}");
        out.println(".initiallyOpen {");
        out.println("    opacity: 1.0;                    ");
        out.println("}");
        out.println(".initiallyClosed {");
        out.println("    opacity: 0.25;");
        out.println("}");
        out.println("A:hover {");
        out.println("    text-decoration: underline;");
        out.println("}");
        out.println("A:link {");
        out.println("    text-decoration: none;");
        out.println("}");
        out.println("A:visited {");
        out.println("    text-decoration: none;");
        out.println("}");
        out._style();
        
        out.body();
        emitBreadcrumbs(out, link);
    }

    public void emitBreadcrumbs(Output out, Link link) throws IOException
    {
        String color = nextBackgroundColor();
        out.div(id("breadcrumbs").class_("log initiallyOpen").style("background-color: " + color));
        
        String uri = link.toString();
        assert uri.startsWith("/");
        StringBuffer sb = new StringBuffer();
        String[] names = uri.split("/");
        for(int i = 1; i < names.length; i++) {
            if(i > 1)
                out.text(" → ");
            sb.append("/").append(names[i]);
            out.a(href(sb.toString())).text(names[i])._a();
        }
        
        out._div();
    }

    @Override
    public void endHtmlPage(Output out, Link link, Object rootPage) throws IOException
    {
        out._body();
        out._html();
    }

    @Override
    public void startEmbed(Output out, int embedDepth, Link link, Object obj) throws IOException
    {
        // Open a <DIV> and generate a unique id for it:
        String parentId = (idStack.isEmpty() ? "" : idStack.getLast());
        String id = freshId();
        String color = nextBackgroundColor();
        out.div(id(id).class_("log initiallyOpen").style("background-color: " + color));
        idStack.add(id);

        // Up-left arrow to go to parent:
        out.a(href("#" + parentId)).text("\u21f1")._a();

        // Down arrow to hide:
        out.span(onclick("toggleId('"+id+"')")).text("\u25bc")._span();

        // Up arrow to magnify:
        if (link != null && link.isValid()) {
            out.a(link).text("\u25b2");
            out._a(link);
        }
    }

    private String nextBackgroundColor()
    {
        return backgroundColors[(currentBackgroundColor++) % backgroundColors.length];
    }

    private String freshId()
    {
        return "id" + (maxId++);
    }

    @Override
    public void endEmbed(Output out, int embedDepth, Link link, Object obj) throws IOException
    {
        out._div();
    }

    @Override
    public boolean handleRequest(String url, Output out) throws IOException
    {
        return false;
    }

}
