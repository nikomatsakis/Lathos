package org.rendersnake.servlet;

import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.ServletException;
import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

import org.rendersnake.FormHandler;
import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;

/**
 * RenderSnakeServlet dispatches GET and POST request to a component which is
 * detected by a RequestToComponentResolver.
 * 
 * @author ernestmicklei
 * 
 */
public class RenderSnakeServlet extends HttpServlet {
    private static final long serialVersionUID = 908531579551509272L;

    private RequestToComponentResolver resolver = new QualifiedClassNameResolver();

    /**
     * On default, use the QualifiedClassNameResolver Example:
     * {context-path}/{servlet-path}/org.rendersnake.HomePage?title=RenderSnake
     */
    @Override
    protected void doGet(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        final HtmlCanvas canvas = new HtmlCanvas(request, response, response.getWriter());
        Renderable component;
        try {
            component = resolver.renderComponentForRequest(request);
            canvas.render(component);
        } catch (Exception ex) {
            component = this.createErrorPage(ex, request);
        }
    }

    @Override
    protected void doPost(HttpServletRequest request, HttpServletResponse response) throws ServletException, IOException {
        try {
            final FormHandler handler = resolver.formHandlerForRequest(request);
            final Class<? extends Renderable> renderComponentClass = handler.handle(request, response);
            // redirect to this component unless a response has been written
            if (!response.isCommitted())
                // TODO : shorter path?
                response.sendRedirect(request.getContextPath() + "/" + request.getServletPath() + "/" + resolver.requestPathForComponentClass(renderComponentClass));
            return;
        } catch (Exception ex) {
            final HtmlCanvas canvas = new HtmlCanvas(request, response, response.getWriter());
            canvas.render(this.createErrorPage(ex, request));
        }
    }

    public RequestToComponentResolver getResolver() {
        return resolver;
    }

    public void setResolver(RequestToComponentResolver resolver) {
        this.resolver = resolver;
    }

    @Override
    public void init(ServletConfig config) throws ServletException {
        // read the resolver
        String className = config.getInitParameter("resolver");
        try {
            Class resolverClass = Class.forName(className);
            RequestToComponentResolver newResolver = (RequestToComponentResolver) (resolverClass.newInstance());
            newResolver.init(config);
            // connect to current
            newResolver.setNextResolver(this.resolver);
            // and replace
            this.setResolver(newResolver);
            super.init(config);
        } catch (Exception e) {
            throw new ServletException("[RenderSnake.init] failed", e);
        }
    }

    private ErrorPage createErrorPage(Exception ex, HttpServletRequest request) {
        ErrorPage pageForError = new ErrorPage();
        pageForError.exception = ex;
        pageForError.requestURL = request.getRequestURL().toString();
        pageForError.resolver = this.resolver;
        return pageForError;
    }
}
