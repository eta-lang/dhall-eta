package org.haskell.types;

import java.math.BigInteger;

public final class Natural extends Number {

	private static final long serialVersionUID = 1L;
	
	private final BigInteger value;
	
	public Natural(BigInteger val) {
		super();
		if (val.compareTo(BigInteger.ZERO) < 0)
			throw new IllegalArgumentException("Natural numbers can not be negative!");
		this.value = val;
	}

	public Natural(long val) {
		this(BigInteger.valueOf(val));
	}
	
	public Natural(int val) {
		this((long)val);
	}
	
	public Natural(String val) {
		this(new BigInteger(val));
	}
	
	@Override
	public int intValue() {
		return value.intValue();
	}

	@Override
	public long longValue() {
		return value.longValue();
	}

	@Override
	public float floatValue() {
		return value.floatValue();
	}

	@Override
	public double doubleValue() {
		return value.doubleValue();
	}

	public BigInteger getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		Natural other = (Natural) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	public String getValueAsStr() {
		return getValue().toString();
	}
	
	@Override
	public String toString() {
		return "Natural ["+getValue()+"]";
	}
	
}
