package org.dhall.core.expr;

import java.util.Optional;

import org.dhall.core.Expr;

public final class ExprMerge<S, A> extends ExprBinary<S, A> implements OptionallyTyped<S, A>  {

	private final Optional<Expr<S,A>> type;
	
	public ExprMerge(Expr<S,A> left, Expr<S, A> right, Optional<Expr<S,A>> type) {
		super(left,right);
		this.type = type;
	}
	
	public Optional<Expr<S,A>> getType() {
		return type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((type == null) ? 0 : type.hashCode());
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
		ExprMerge<S, A> other = (ExprMerge<S, A>) obj;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprMerge [ left=" + getLeft() + ", right=" + getRight() + ", type=" + type + "]";
	}
	
	
}	
