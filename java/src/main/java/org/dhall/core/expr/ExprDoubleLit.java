package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprDoubleLit<S,A> extends Expr<S, A> implements Literal<Double>{
	final Double value;

	public ExprDoubleLit(Double value) {
		super();
		this.value = value;
	}
	
	@Override
	public Double getValue() {
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
		ExprDoubleLit<S,A> other = (ExprDoubleLit<S,A>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprDoubleLit [value=" + value + "]";
	}

	
	
}
