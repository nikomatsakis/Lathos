package org.rendersnake.servlet;

import javax.servlet.ServletConfig;
import javax.servlet.http.HttpServletRequest;

import org.rendersnake.FormHandler;
import org.rendersnake.Renderable;

public interface RequestToComponentResolver extends Renderable {
    
    void init(ServletConfig config);
    
    Renderable renderComponentForRequest(HttpServletRequest request);
    
    FormHandler formHandlerForRequest(HttpServletRequest request);
    
    String requestPathForComponentClass(Class<? extends Renderable> componentClass);
    
    RequestToComponentResolver getNextResolver();
    
    RequestToComponentResolver setNextResolver(RequestToComponentResolver resolver);
}
