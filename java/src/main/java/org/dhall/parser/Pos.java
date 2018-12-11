package org.dhall.parser;

public class Pos extends Number{

private static final long serialVersionUID = 1L;
	
	private final int value;
	
	public Pos(int val) {
		super();
		if (val <= 0)
			throw new IllegalArgumentException("Position must be a positive integer!");
		this.value = val;
	}

	public Pos(String val) {
		this(Integer.valueOf(val));
	}
	
	@Override
	public int intValue() {
		return value;
	}

	@Override
	public long longValue() {
		return value;
	}

	@Override
	public float floatValue() {
		return value;
	}

	@Override
	public double doubleValue() {
		return value;
	}

	public int getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + value;
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
		Pos other = (Pos) obj;
		if (value != other.value)
			return false;
		return true;
	}

	@Override
		public String toString() {
			return "Pos["+value+"]";
		}
}
