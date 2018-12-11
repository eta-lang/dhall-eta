package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantEQ<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantEQ(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
