package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalTimes<S, A> extends ExprBinary<S, A> {

	public ExprNaturalTimes(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
