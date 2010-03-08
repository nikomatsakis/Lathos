package com.smallcultfollowing.lathos;

public final class LList<E, F> {
	final E elemE;
	final F elemF;
	final LList<E, F> next;
	
	public LList(E elemE, F elemF, LList<E, F> next) {
		this.elemE = elemE;
		this.elemF = elemF;
		this.next = next;
	}
	
	public static <E, F> boolean containsE(LList<E, F> lst, E elem) {
		while(lst != null) {
			if(lst.elemE.equals(elem))
				return true;
			lst = lst.next;
		}
		return false;
	}
	
	public static <E> LList<E, Void> addIfNotContained(E itemE, LList<E, Void> list) {
		if(containsE(list, itemE)) return list;
		return add(itemE, null, list);
	}
	
	public static <E, F> LList<E, F> add(E itemE, F itemF, LList<E, F> list) {
		return new LList<E, F>(itemE, itemF, list);
	}

	public static <E> LList<E, Void> llist(E... ids) {
		LList<E, Void> result = null;
		for (int i = ids.length - 1; i >= 0; i--) {
			result = new LList<E, Void>(ids[i], null, result);
		}
		return result;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((elemE == null) ? 0 : elemE.hashCode());
		result = prime * result + ((elemF == null) ? 0 : elemF.hashCode());
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
		LList<?,?> other = (LList<?,?>) obj;
		if (elemE == null) {
			if (other.elemE != null)
				return false;
		} else if (!elemE.equals(other.elemE))
			return false;
		if (elemF == null) {
			if (other.elemF != null)
				return false;
		} else if (!elemF.equals(other.elemF))
			return false;
		if (next == null) {
			if (other.next != null)
				return false;
		} else if (!next.equals(other.next))
			return false;
		return true;
	}
	
}
