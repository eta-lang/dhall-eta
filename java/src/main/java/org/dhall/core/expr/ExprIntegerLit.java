package org.dhall.core.expr;

import java.math.BigInteger;

import org.dhall.core.Expr;

public final class ExprIntegerLit<S,A> extends Expr<S, A> implements Literal<BigInteger>{
	final BigInteger integer;

	public ExprIntegerLit(BigInteger integer) {
		super();
		this.integer = integer;
	}
	
	@Override
	public BigInteger getValue() {
		return integer;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((integer == null) ? 0 : integer.hashCode());
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
		ExprIntegerLit<S,A> other = (ExprIntegerLit<S,A>) obj;
		if (integer == null) {
			if (other.integer != null)
				return false;
		} else if (!integer.equals(other.integer))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprIntegerLit [integer=" + integer + "]";
	}

	
}
