package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantAnd<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantAnd(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
