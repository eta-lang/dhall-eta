package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantInterpolate<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantInterpolate(Expr<S, A> expression, Expr<S, A> type) {
		super(expression, type);
	}

}
