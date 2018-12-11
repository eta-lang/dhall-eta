package org.haskell.types.either;

import org.haskell.types.Either;

public final class Left<L,R> extends Either<L,R> {
	private final L value;

	public Left(L left) {
		super();
		this.value = left;
	}

	public L getValue() {
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
		@SuppressWarnings("unchecked")
		Left<L,R> other = (Left<L,R>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}
	@Override
	public String toString() {
		return "Left [" + value.toString() +"]";
	}
}
