package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidListType<S,A> extends AnnotationMessage<S,A> {
	
	public InvalidListType(Expr<S, A> annot) {
		super(annot);
	}

	public Expr<S, A> getType() {
		return getAnnotation();
	}
	
}
