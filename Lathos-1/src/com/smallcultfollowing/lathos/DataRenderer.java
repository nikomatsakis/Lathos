package com.smallcultfollowing.lathos;

public interface DataRenderer {
	
	/** Attempts to add a representation of {@code object} into
	 *  {@code line}.  If {@code object} is not handled by this
	 *  object, returns {@code false}. 
	 *  
	 *  @param line line to add representation of {@code object} to
	 *  @param object the object to render 
	 *  @render true if the object was added */
	public boolean addToLine(Line line, Object object);
	
}