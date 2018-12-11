package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprSome<S, A> extends ExprUnary<S,A> {

	public ExprSome(Expr<S, A> value) {
		super(value);
	}

	public Expr<S, A> getValue() {
		return getSubexpr();
	}

	
}
