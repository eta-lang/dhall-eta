package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolAnd<S, A> extends ExprBinary<S, A> {

	public ExprBoolAnd(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
