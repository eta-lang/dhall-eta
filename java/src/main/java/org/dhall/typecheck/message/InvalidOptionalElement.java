package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidOptionalElement<S,A> extends AnnotatedExpressionMismatchMessage<S,A> {

	public InvalidOptionalElement(Expr<S, A> expectedType, 
			Expr<S, A> expression, Expr<S, A> actualType) {
		super(expression, expectedType, actualType);
	}
	
}
