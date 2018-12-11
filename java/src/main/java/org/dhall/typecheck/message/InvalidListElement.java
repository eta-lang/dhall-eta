package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidListElement<S,A> extends AnnotatedExpressionMismatchMessage<S,A> {
    
	private final Integer index;
	
	public InvalidListElement(Integer index, Expr<S, A> expectedType, Expr<S, A> elemExpr, Expr<S, A> actualType) {
		super(elemExpr, expectedType, actualType);
		this.index = index;
	}

	public Integer getIndex() {
		return index;
	}

}
