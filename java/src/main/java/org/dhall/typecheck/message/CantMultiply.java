package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantMultiply<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantMultiply(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
