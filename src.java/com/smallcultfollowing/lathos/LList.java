package com.smallcultfollowing.lathos;

public final class LList<E> {
	final E elem;
	final LList<E> next;
	
	public LList(E logId, LList<E> next) {
		this.elem = logId;
		this.next = next;
	}
	
	public boolean contains(E logId) {
		if(this.elem.equals(logId))
			return true;
		if(this.next != null)
			return this.next.contains(logId);
		return false;
	}
	
	public LList<E> prefixIfNotContained(E logId) {
		if(contains(logId)) return this;
		return prefix(logId);
	}
	
	public LList<E> prefix(E logId) {
		return new LList<E>(logId, this);
	}
}
