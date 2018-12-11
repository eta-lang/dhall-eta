package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public abstract class AnnotatedExpressionMismatchMessage<S,A> extends AnnotatedExpressionMessage<S,A> {

	private final Expr<S,A> expectedAnnotation;
	
	public AnnotatedExpressionMismatchMessage(Expr<S, A> expression, 
			Expr<S, A> expectedType, Expr<S, A> actualType) {
		super(expression, actualType);
		this.expectedAnnotation = expectedType;
	}
	
	public Expr<S, A> getExpectedAnnotation() {
		return expectedAnnotation;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((expectedAnnotation == null) ? 0 : 
										expectedAnnotation.hashCode());
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
		AnnotatedExpressionMismatchMessage<S,A> other = 
			(AnnotatedExpressionMismatchMessage<S,A>) obj;
		if (expectedAnnotation == null) {
			if (other.expectedAnnotation != null)
				return false;
		} else if (!expectedAnnotation.equals(other.expectedAnnotation))
			return false;
		return true;
	}


}
