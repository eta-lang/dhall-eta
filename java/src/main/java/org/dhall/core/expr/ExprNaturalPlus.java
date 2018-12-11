package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalPlus<S, A> extends ExprBinary<S, A> {

	public ExprNaturalPlus(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
