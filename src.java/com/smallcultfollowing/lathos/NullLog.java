package com.smallcultfollowing.lathos;

public class NullLog implements Log {

	@Override
	public LogId freshLogId() {
		return null;
	}

	@Override
	public LogId indent(String fmt, Object... args) {
		return null;
	}

	@Override
	public LogId log(String fmt, Object... args) {
		return null;
	}

	@Override
	public void undent(LogId top) {
	}

}
