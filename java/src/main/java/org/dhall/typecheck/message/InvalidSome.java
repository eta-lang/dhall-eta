package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidSome<S,A> extends AnnotatedExpressionMessage<S,A> {

	private final Expr<S,A> kind;
	
	public InvalidSome(Expr<S, A> expression, Expr<S, A> type, Expr<S, A> kind) {
		super(expression,type);
		this.kind = kind;
	}
	
	public Expr<S, A> getKind() {
		return kind;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((kind == null) ? 0 : kind.hashCode());
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
		InvalidSome<S,A> other = (InvalidSome<S,A>) obj;
		if (kind == null) {
			if (other.kind != null)
				return false;
		} else if (!kind.equals(other.kind))
			return false;
		return true;
	}
	
	
}
