package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolOr<S, A> extends ExprBinary<S, A> {

	public ExprBoolOr(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
