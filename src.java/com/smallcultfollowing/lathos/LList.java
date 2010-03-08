package com.smallcultfollowing.lathos;

public final class LList<E> {
	final E elem;
	final LList<E> next;
	
	public LList(E logId, LList<E> next) {
		this.elem = logId;
		this.next = next;
	}
	
	public static <E> boolean contains(LList<E> lst, E elem) {
		while(lst != null) {
			if(lst.elem.equals(elem))
				return true;
			lst = lst.next;
		}
		return false;
	}
	
	public static <E> LList<E> addIfNotContained(E item, LList<E> list) {
		if(contains(list, item)) return list;
		return add(item, list);
	}
	
	public static <E> LList<E> add(E item, LList<E> list) {
		return new LList<E>(item, list);
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((elem == null) ? 0 : elem.hashCode());
		result = prime * result + ((next == null) ? 0 : next.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		LList other = (LList) obj;
		if (elem == null) {
			if (other.elem != null)
				return false;
		} else if (!elem.equals(other.elem))
			return false;
		if (next == null) {
			if (other.next != null)
				return false;
		} else if (!next.equals(other.next))
			return false;
		return true;
	}
	
	public static <E> LList<E> llist(E... ids) {
		LList<E> result = null;
		for (int i = ids.length - 1; i >= 0; i--) {
			result = new LList<E>(ids[i], result);
		}
		return result;
	}
	
}
