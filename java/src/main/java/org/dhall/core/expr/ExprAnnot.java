package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprAnnot<S, A> extends ExprBinary<S, A> implements Typed<S, A> {

	public ExprAnnot(Expr<S, A> name, Expr<S, A> type) {
		super(name,type);
	}

	public Expr<S, A> getName() {
		return getLeft();
	}

	public Expr<S, A> getType() {
		return getRight();
	}

	@Override
	protected String getLeftLabel() {
		return "name";
	}
	
	@Override
	protected String getRightLabel() {
		return "type";
	}
}	
