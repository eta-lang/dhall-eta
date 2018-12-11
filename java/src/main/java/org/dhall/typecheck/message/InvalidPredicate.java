package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidPredicate<S,A> extends AnnotatedExpressionMessage<S,A> {

	public InvalidPredicate(Expr<S, A> expression, Expr<S, A> actualType) {
		super(expression,actualType);
	}
	
}
