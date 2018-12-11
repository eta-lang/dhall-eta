package org.dhall.core.expr;

import org.dhall.core.Const;
import org.dhall.core.Expr;

public final class ExprConst<S,A> extends Expr<S, A> {
	
	final Const constant;

	public ExprConst(Const c) {
		super();
		this.constant = c;
	}

	public Const getConstant() {
		return constant;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((constant == null) ? 0 : constant.hashCode());
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
		ExprConst<S,A> other = (ExprConst<S,A>) obj;
		if (constant == null) {
			if (other.constant != null)
				return false;
		} else if (!constant.equals(other.constant))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprConst [constant=" + constant + "]";
	}
	
	
}
