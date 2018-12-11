package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolLit<S,A> extends Expr<S, A> implements Literal<Boolean>{
	final boolean bool;

	public ExprBoolLit(Boolean c) {
		super();
		this.bool = c;
	}
	@Override
	public Boolean getValue() {
		return bool;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + (bool ? 1231 : 1237);
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
		ExprBoolLit<S,A> other = (ExprBoolLit<S,A>) obj;
		if (bool != other.bool)
			return false;
		return true;
	}

	
	
}
