package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprTextAppend<S, A> extends ExprBinary<S, A> {

	public ExprTextAppend(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
