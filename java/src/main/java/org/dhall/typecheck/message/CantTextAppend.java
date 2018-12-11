package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantTextAppend<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantTextAppend(Expr<S,A> left, Expr<S,A> right) {
		super(left, right);
	}

}
