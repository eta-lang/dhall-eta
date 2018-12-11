package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprApp<S, A> extends ExprBinary<S, A> {

	public ExprApp(Expr<S, A> function, Expr<S, A> argument) {
		super(function,argument);
	}

	public Expr<S, A> getFunction() {
		return getLeft();
	}

	public Expr<S, A> getArgument() {
		return getRight();
	}

	@Override
	protected String getLeftLabel() {
		return "function";
	}
	
	@Override
	protected String getRightLabel() {
		return "argument";
	}
}	
