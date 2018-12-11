package org.dhall.core.expr;

import org.dhall.core.Expr;
import org.dhall.core.Var;

public final class ExprVar<S, A> extends Expr<S, A> {
	private final Var var;

	public ExprVar(Var var) {
		super();
		this.var = var;
	}

	public Var getVar() {
		return var;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((var == null) ? 0 : var.hashCode());
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
		ExprVar<S,A> other = (ExprVar<S,A>) obj;
		if (var == null) {
			if (other.var != null)
				return false;
		} else if (!var.equals(other.var))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprVar [var=" + var + "]";
	}
	
	
}
