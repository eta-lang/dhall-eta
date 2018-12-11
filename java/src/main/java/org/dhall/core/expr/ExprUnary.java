package org.dhall.core.expr;

import org.dhall.core.Expr;

public abstract class ExprUnary<S, A> extends Expr<S, A> {
	private final Expr<S, A> subExpr;

	public ExprUnary(Expr<S, A> subExpr) {
		super();
		this.subExpr = subExpr;
	}

	public Expr<S, A> getSubexpr() {
		return subExpr;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((subExpr == null) ? 0 : subExpr.hashCode());
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
		ExprUnary<S, A> other = (ExprUnary<S, A>) obj;
		if (subExpr == null) {
			if (other.subExpr != null)
				return false;
		} else if (!subExpr.equals(other.subExpr))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return getClass().getSimpleName()+" [subexpr="+subExpr.toString()+"]";
	}
	
}
