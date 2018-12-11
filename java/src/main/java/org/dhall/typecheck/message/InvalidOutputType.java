package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidOutputType<S,A> extends AnnotationMessage<S,A> {
	
	public InvalidOutputType(Expr<S, A> annot) {
		super(annot);
	}

	public Expr<S, A> getType() {
		return getAnnotation();
	}
	
}
