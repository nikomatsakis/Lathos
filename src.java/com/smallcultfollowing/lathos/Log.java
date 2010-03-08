package com.smallcultfollowing.lathos;

public interface Log {

	/** 
	 * Returns a LogId guaranteed to be unique for this Log instance.
	 * May return {@code null} if logging is disabled! */
	public abstract LogId freshLogId();

	/** 
	 * Generates a log message.  The {@code fmt} string only supports
	 * the {@code %s} and {@code %%} specifies.  If any of the
	 * objects in {@code args} implements the {@link Loggable} interface,
	 * then a link will be generated for it.  Otherwise, the result of
	 * the object's {@code toString()} method will be placed inline.
	 */
	public abstract LogId log(String fmt, Object... args);

	/** 
	 * Generates a message and causes any further log messages to
	 * be placed as its children until the next call to 
	 * {@link #undent(LogId)}.  The return value must be passed
	 * to {@link #undent(LogId)} to assert proper nesting.
	 * May return {@code null}, particularly when logging is
	 * disabled.
	 * 
	 * Suggested pattern:
	 * <pre>
	 *   LogId id = log.indent(...);
	 *   try {
	 *     // indented code
	 *   } finally {
	 *     log.undent(id);
	 *   }
	 * </pre>
	 * 
	 * @see #log(String, Object...)
	 */
	public abstract LogId indent(String fmt, Object... args);

	/** @see #indent(String, Object...) */
	public abstract void undent(LogId top);

}