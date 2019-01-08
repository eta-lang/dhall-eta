package org.dhall.common.types.either;

import org.dhall.common.types.Either;

public final class Right<L,R> extends Either<L, R> {
	private final R value;

	public Right(R right) {
		super();
		this.value = right;
	}

	public R getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		return result;
	}

	@SuppressWarnings("unchecked")
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		Right<L,R> other = (Right<L,R>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	@Override
	public String toString() {
		return "Right [" + value.toString() +"]";
	}
}
