package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantAdd<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantAdd(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
