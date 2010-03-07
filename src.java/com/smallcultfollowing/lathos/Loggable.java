package com.smallcultfollowing.lathos;

import java.util.Map;

public interface Loggable {
	/** Returns a globally unique log identifier. */
	public LogId logId();
	
	/** Returns a descriptive heading for this object. */
	public String logHeading();
	
	/** Returns the sub-fields etc of this object. */
	public Map<String, Object> logFields();
}
