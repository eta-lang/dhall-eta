package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprCombine<S, A> extends ExprBinary<S, A> {

	public ExprCombine(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
