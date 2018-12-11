package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class AnnotMismatch<S,A> extends AnnotatedExpressionMismatchMessage<S,A> {

	public AnnotMismatch(Expr<S, A> expression, Expr<S, A> expectedType, 
						 Expr<S, A> actualType) {
		super(expression, expectedType, actualType);
	}
	
}
