package com.smallcultfollowing.lathos;

public interface LogInter {

	public abstract LogId freshLogId();

	public abstract LogId log(String fmt, Object... args);

	public abstract LogId indent(String fmt, Object... args);

	public abstract void undent(LogId top);

}