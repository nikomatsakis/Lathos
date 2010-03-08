package com.smallcultfollowing.lathos;


public abstract class LogLink {

	/** 
	 * Sends a log message defining new nodes to the server.  
	 * Returns a list containing any ids which were linked to
	 * but which did not already exist.
	 * 
	 * @param msg An erlang term defining the new log node(s). */
	public abstract LList<LogId, Void> createNode(String msg);

}
