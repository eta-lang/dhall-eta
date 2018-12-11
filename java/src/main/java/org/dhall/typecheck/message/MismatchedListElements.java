package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class MismatchedListElements<S,A> extends AnnotatedExpressionMismatchMessage<S,A> {
    
	private final Integer index;
	
	public MismatchedListElements(Integer index, Expr<S, A> expectedType, 
			Expr<S, A> elemExpr, Expr<S, A> actualType) {
		super(elemExpr, expectedType,actualType );
		this.index = index;
	}

	public Integer getIndex() {
		return index;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((index == null) ? 0 : index.hashCode());
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
		MismatchedListElements<S,A> other = (MismatchedListElements<S,A>) obj;
		if (index == null) {
			if (other.index != null)
				return false;
		} else if (!index.equals(other.index))
			return false;
		return true;
	}

	
}
