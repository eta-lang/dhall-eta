package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class NotAFunction<S,A> extends AnnotatedExpressionMessage<S,A> {
	
	public NotAFunction(Expr<S, A> expression, Expr<S, A> annot) {
		super(expression,annot);
	}

}
