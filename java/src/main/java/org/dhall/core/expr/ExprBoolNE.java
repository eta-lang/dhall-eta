package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolNE<S, A> extends ExprBinary<S, A> {

	public ExprBoolNE(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
