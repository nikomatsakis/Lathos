package com.smallcultfollowing.lathos;

import java.util.Map;

public interface Loggable {
	/** 
	 * Optionally returns a globally unique log identifier.
	 * 
	 * <p>If an identifier is returned, then multiple instances of
	 * {@code Loggable} which return the same identifier will all
	 * be linked to the same node, which also improves performance
	 * since their fields are only recorded once.  If any fields of
	 * the object change, the identifier which is returned must also
	 * change: the easiest way to achieve this is simply to change the
	 * {@link LogId#version} field.  
	 * 
	 * <p>If {@code null} is returned, then a complete copy of the
	 * object will be serialized each time it appears in the log.
	 * This is convenient for small or frequently mutated data structures 
	 * where there is no need to have multiple links to the same item. 
	 */
	public LogId logId();
	
	/** Returns a descriptive heading for this object. */
	public String logHeading();
	
	/** Returns the sub-fields etc of this object. */
	public Map<String, Object> logFields();
}
