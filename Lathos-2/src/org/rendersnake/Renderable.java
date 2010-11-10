package org.rendersnake;

import java.io.IOException;
/**
 * Implementors can be rendered using a HtmlCanvas.
 * <p/>
 * An IOException is thrown when the canvas cannot write its tags.
 * 
 * @author ernestmicklei
 */
public interface Renderable {
    /**
     * Render the receiver using the canvas.
     * @param canvas
     * @throws IOException
     */
    void renderOn(HtmlCanvas canvas) throws IOException;
}
