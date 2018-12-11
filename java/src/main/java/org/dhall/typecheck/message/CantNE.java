package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class CantNE<S,A> extends AnnotatedExpressionMessage<S,A> {

	public CantNE(Expr<S,A> left, Expr<S,A> right) {
		super(left, right);
	}

}
