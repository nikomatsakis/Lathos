package org.rendersnake.servlet;

import java.io.IOException;

import javax.servlet.ServletConfig;
import javax.servlet.http.HttpServletRequest;

import org.rendersnake.FormHandler;
import org.rendersnake.HtmlCanvas;
import org.rendersnake.Renderable;

public class QualifiedClassNameResolver implements RequestToComponentResolver {

    private RequestToComponentResolver nextResolver;
    
    public Renderable renderComponentForRequest(HttpServletRequest request) {
        String className = request.getPathInfo().substring(1);
        try {
            Class<?> theClass = Class.forName(className);
            return (Renderable) (theClass.newInstance());

        } catch (ClassNotFoundException ex) {
            if (nextResolver != null)
                return this.nextResolver.renderComponentForRequest(request);
            else
                throw new RuntimeException("[QualifiedClassNameResolver.renderComponentForRequest] ClassNotFoundException: "+ex.getMessage(),ex);
        } catch (InstantiationException ex) {
            throw new RuntimeException("[QualifiedClassNameResolver.renderComponentForRequest] InstantiationException: "+ex.getMessage(),ex);
        } catch (IllegalAccessException ex) {
            throw new RuntimeException("[QualifiedClassNameResolver.renderComponentForRequest] IllegalAccessException: "+ex.getMessage(),ex);
        }
    }

    public FormHandler formHandlerForRequest(HttpServletRequest request) {
        String className = request.getPathInfo().substring(1);
        try {
            Class<?> theClass = Class.forName(className);
            return (FormHandler) (theClass.newInstance());

        } catch (ClassNotFoundException ex) {
            // TODO
            throw new RuntimeException(ex);
        } catch (InstantiationException ex) {
            throw new RuntimeException(ex);
        } catch (IllegalAccessException ex) {
            throw new RuntimeException(ex);
        }
    }    

    // For debugging purposes
    public void renderOn(HtmlCanvas canvas) throws IOException {
        canvas.text(this.toString());
        if (nextResolver != null) {
            nextResolver.renderOn(canvas);
        }
    }    

	public String requestPathForComponentClass(Class<? extends Renderable> componentClass) {
		return componentClass.getName();
	}

    /* (non-Javadoc)
     * @see org.rendersnake.RequestToComponentResolver#init(javax.servlet.ServletConfig)
     */
    public void init(ServletConfig config) {
    }

    /* (non-Javadoc)
     * @see org.rendersnake.RequestToComponentResolver#nextResolver()
     */
    public RequestToComponentResolver getNextResolver() {
        return getNextResolver();
    }

    public RequestToComponentResolver setNextResolver(RequestToComponentResolver resolver) {
        return this.nextResolver = resolver;
    }
}
