package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListAppend<S, A> extends ExprBinary<S, A> {

	public ExprListAppend(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
