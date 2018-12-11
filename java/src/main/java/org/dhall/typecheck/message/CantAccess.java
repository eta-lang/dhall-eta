package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantAccess<S,A> extends AnnotatedNameMessage<S,A> {
	
	private final Expr<S, A> expectedType;
	

	public CantAccess(String name, 
			Expr<S, A> expectedType, Expr<S, A> actualType) {
		super(name, actualType);
		this.expectedType = expectedType;
	}
	
	public Expr<S, A> getExpectedType() {
		return expectedType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((expectedType == null) ? 0 : expectedType.hashCode());
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
		CantAccess<S,A> other = (CantAccess<S,A>) obj;
		if (expectedType == null) {
			if (other.expectedType != null)
				return false;
		} else if (!expectedType.equals(other.expectedType))
			return false;
		return true;
	}
}
