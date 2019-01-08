package org.dhall.core.expr;

import org.dhall.common.types.Natural;
import org.dhall.core.Expr;

public final class ExprNaturalLit<S,A> extends Expr<S, A> implements Literal<Natural> {
	final Natural nat;

	public ExprNaturalLit(Natural nat) {
		super();
		this.nat = nat;
	}

	@Override
	public Natural getValue() {
		return nat;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((nat == null) ? 0 : nat.hashCode());
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
		ExprNaturalLit<S,A> other = (ExprNaturalLit<S,A>) obj;
		if (nat == null) {
			if (other.nat != null)
				return false;
		} else if (!nat.equals(other.nat))
			return false;
		return true;
	}

	
	@Override
	public String toString() {
		return "ExprNaturalLit ["+nat+"]";
	}
	
}
