package com.smallcultfollowing.lathos;

import static org.rendersnake.AttributesFactory.class_;
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

    private final String[] backgroundColors = new String[] { "C4D9FF", "BACEF2", "B1C4E5", "A7B7D9", "9DAECC",
            "93A3BF", "8998B2" };

    @Override
    public void startHtmlPage(Output out, Link link, Object rootPage) throws IOException
    {
        out.html();

        out.script(AttributesFactory.type("text/javascript"));
        out.rawLine("function toggleId(id)");
        out.rawLine("{");
        out.rawLine("    var target = document.getElementById(id);");
        out.rawLine("    var kids = target.childNodes;");
        out.rawLine("    var openedKids = false;");
        out.rawLine("    var closedKids = false;");
        out.rawLine("    for(var i = 0; (i < kids.length); i++) {");
        out.rawLine("        var kid = kids[i];");
        out.rawLine("        if(");
        out.rawLine("            kid.className == 'content'");
        out.rawLine("        ) {");
        out.rawLine("            if(kid.style.display == 'none') {");
        out.rawLine("                kid.style.display = 'block';");
        out.rawLine("                openedKids = true;");
        out.rawLine("            } else {");
        out.rawLine("                kid.style.display = 'none';");
        out.rawLine("                closedKids = true;");
        out.rawLine("            }");
        out.rawLine("        }");
        out.rawLine("    }");
        out.rawLine("    ");
        out.rawLine("    if(openedKids) {");
        out.rawLine("        target.style.opacity = 1.0;");
        out.rawLine("    } else if (closedKids) {");
        out.rawLine("        target.style.opacity = 0.25;");
        out.rawLine("    }");
        out.rawLine("}");
        out._script();

        out.style();
        out.rawLine("DIV.controls {");
        out.rawLine("    float: right;");
        out.rawLine("    font-size: small;");
        out.rawLine("}");
        out.rawLine("DIV.log {");
        out.rawLine("    border-width: thin;");
        out.rawLine("    border-style: solid;");
        out.rawLine("    margin-top: .1cm;");
        out.rawLine("    margin-bottom: .1cm;");
        out.rawLine("    margin-left: .3cm;                    ");
        out.rawLine("}");
        out.rawLine(".initiallyOpen {");
        out.rawLine("    opacity: 1.0;                    ");
        out.rawLine("}");
        out.rawLine(".initiallyClosed {");
        out.rawLine("    opacity: 0.25;");
        out.rawLine("}");
        out.rawLine("A:hover {");
        out.rawLine("    text-decoration: underline;");
        out.rawLine("}");
        out.rawLine("A:link {");
        out.rawLine("    text-decoration: none;");
        out.rawLine("}");
        out.rawLine("A:visited {");
        out.rawLine("    text-decoration: none;");
        out.rawLine("}");
        out._style();

        out.body();
        emitBreadcrumbs(out, link);
    }

    public void emitBreadcrumbs(Output out, Link link) throws IOException
    {
        String color = backgroundColor(0);
        out.div(id("breadcrumbs").class_("log initiallyOpen").style("background-color: " + color));

        String[] names = BaseLink.decodeIntoNames(link.toString());
        for (int i = 0; i < names.length; i++) {
            if (i > 0)
                out.text(" → ");
            Link linkSoFar = new BaseLink(names, i + 1);
            out.a(href(linkSoFar.toString())).text(names[i])._a();
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
        // String parentId = (idStack.isEmpty() ? "" : idStack.getLast());
        String id = freshId();
        String color = backgroundColor(embedDepth);
        out.div(id(id).class_("log initiallyOpen").style("background-color: " + color));
        idStack.push(id);

        out.div(class_("controls").style("background-color: " + color));

        // Up-left arrow to go to parent:
        // out.a(href("#" + parentId)).text("\u21f1")._a();

        // Down arrow to hide:
        out.span(onclick("toggleId('" + id + "')")).text("(hide)")._span();

        // Up arrow to magnify:
        if (link != null && link.isValid()) {
            out.a(link).text(" (focus)");
            out._a(link);
        }

        out._div();

        out.div(class_("content"));
    }

    private String backgroundColor(int depth)
    {
        return backgroundColors[depth % backgroundColors.length];
    }

    private String freshId()
    {
        return "id" + (maxId++);
    }

    @Override
    public void endEmbed(Output out, int embedDepth, Link link, Object obj) throws IOException
    {
        out._div();
        out._div();
        idStack.pop();
    }

    @Override
    public boolean handleRequest(String url, Output out) throws IOException
    {
        return false;
    }

    @Override
    public void startSubPage(Output output, int embedDepth) throws IOException
    {
        startEmbed(output, embedDepth, null, null);
    }

    @Override
    public void endSubPage(Output output, int embedDepth) throws IOException
    {
        endEmbed(output, embedDepth, null, null);
    }

}
