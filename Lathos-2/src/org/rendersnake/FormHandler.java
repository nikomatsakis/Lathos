package org.rendersnake;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;

public interface FormHandler {

	Class<? extends Renderable> handle(HttpServletRequest request, HttpServletResponse response);
}
