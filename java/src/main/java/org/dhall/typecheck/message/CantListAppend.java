package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantListAppend<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantListAppend(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
