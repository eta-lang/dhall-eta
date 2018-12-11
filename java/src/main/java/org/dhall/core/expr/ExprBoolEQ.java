package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolEQ<S, A> extends ExprBinary<S, A> {

	public ExprBoolEQ(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
