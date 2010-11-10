package org.rendersnake.servlet;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;

import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;

public class ErrorPage implements Renderable {

    public Exception exception;
    public String    requestURL;
    public RequestToComponentResolver resolver;
    
    public void renderOn(HtmlCanvas canvas) throws IOException { // @formatter: off
        canvas.html()
            .body()
            .h1().text("[renderSnake] Unable to render page")._h1();
        
        if (null != exception) {
            this.renderStackTraceOn(canvas);
        }
        canvas.getPageContext().renderForErrorOn(canvas);
        this.renderHttpRequestOn(canvas);
        if (resolver != null) {
            resolver.renderOn(canvas);
        }
        canvas._body()
            ._html();
    }
    // Pre: exception not null
    public void renderStackTraceOn(HtmlCanvas html) throws IOException { // @formatter: off
        StringWriter sw = new StringWriter();
        PrintWriter buffer = new PrintWriter(sw);
        exception.printStackTrace(buffer);
        html.pre().text(sw.toString(), true)._pre();
    }
    public void renderHttpRequestOn(HtmlCanvas canvas) throws IOException {
        canvas
            .table()
                .tr()
                    .td().text("query")._td()
                    .td().text(canvas.request.getQueryString())._td()
                ._tr()
            ._table();
    }
}
