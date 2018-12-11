package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprConstructors<S, A> extends ExprUnary<S, A> {

	public ExprConstructors(Expr<S, A> subExpr) {
		super(subExpr);
	}

	
}
