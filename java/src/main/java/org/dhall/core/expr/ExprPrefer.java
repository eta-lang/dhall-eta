package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprPrefer<S, A> extends ExprBinary<S, A> {

	public ExprPrefer(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
