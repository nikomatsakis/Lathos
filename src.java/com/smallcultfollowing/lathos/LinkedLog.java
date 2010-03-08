package com.smallcultfollowing.lathos;


public class LinkedLog implements Log {
	private int counter;
	private LList<LogId, Void> idStack = new LList<LogId, Void>(LogId.index, null, null);
	private final LogLink link;
	
	public LinkedLog(LogLink link) {
		this.link = link;
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#freshLogId()
	 */
	public LogId freshLogId() {
		int value = counter++;
		return new LogId("Log"+value, 1);
	}
	
	private void achieveClosure(LList<LogId, Void> missing, LList<LogId, Loggable> links) {
		while(missing != null) {
			LogSerializer serializer = new LogSerializer(this);
			LList<LogId, Loggable> nextLinks = null;
			
			while(links != null) {
				LogId logId = links.elemE;
				if(LList.containsE(missing, logId))
					nextLinks = serializer.appendErlangNodesForLoggable(nextLinks, logId, links.elemF);
				links = links.next;
			}
			
			links = nextLinks;
			missing = link.createNode(serializer.result());
		}
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#log(java.lang.String, java.lang.Object)
	 */
	public LogId log(String fmt, Object... args) {
		LogId id = freshLogId();
		LogSerializer serializer = new LogSerializer(this);		
		LList<LogId, Loggable> links = serializer.appendErlangNodeForLogMessage(null, id, idStack.elemE, fmt, args);
		LList<LogId, Void> missing = link.createNode(serializer.result());
		achieveClosure(missing, links);
		return id;
	}

	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#indent(java.lang.String, java.lang.Object)
	 */
	public LogId indent(String fmt, Object... args) {
		LogId id = log(fmt, args);
		idStack = LList.add(id, null, idStack);
		return idStack.elemE;
	}
	
	/* (non-Javadoc)
	 * @see com.smallcultfollowing.lathos.LogInter#undent(com.smallcultfollowing.lathos.LogId)
	 */
	public void undent(LogId top) {
		assert top == idStack.elemE : "Unmatched indent and undent";
		idStack = idStack.next;
	}

}
