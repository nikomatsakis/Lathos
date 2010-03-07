package com.smallcultfollowing.lathos;

public final class LogId {
	public final String name;
	public final int version;

	public LogId(String name, int version) {
		super();
		this.name = name;
		this.version = version;
	}

	public String toString() {
		return name + "." + version;
	}
	
	public void appendAsErlangTerm(StringBuilder sb) {
		sb.append("{id, ");
		Util.appendEscapedString(sb, name);
		sb.append(", ").append(version).append("}");
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + version;
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
		LogId other = (LogId) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (version != other.version)
			return false;
		return true;
	}

	public static final LogId index = new LogId("index", 1); 
}
