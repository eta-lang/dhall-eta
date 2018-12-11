package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNote<S, A> extends ExprUnary<S,A> {

	private final S annotation;
	
	public ExprNote(S s,Expr<S, A> value) {
		super(value);
		this.annotation= s;
	}

	public Expr<S, A> getValue() {
		return getSubexpr();
	}

	public S getAnnotation() {
		return annotation;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((annotation == null) ? 0 : annotation.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		ExprNote<S, A> other = (ExprNote<S, A>) obj;
		if (annotation == null) {
			if (other.annotation != null)
				return false;
		} else if (!annotation.equals(other.annotation))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprNote [annotation="+annotation+", subexpr="+getSubexpr()+"]";
	}
	
}
