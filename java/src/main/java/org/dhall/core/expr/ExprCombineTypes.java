package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprCombineTypes<S, A> extends ExprBinary<S, A> {

	public ExprCombineTypes(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
