package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprImportAlt<S, A> extends ExprBinary<S, A> {

	public ExprImportAlt(Expr<S, A> left, Expr<S, A> right) {
		super(left,right);
	}

	
}	
