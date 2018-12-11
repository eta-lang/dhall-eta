package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantOr<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantOr(Expr<S,A> left, Expr<S,A> right) {
		super(left, right);
	}

}
