package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidOptionalType<S,A> extends AnnotationMessage<S,A> {
	
	public InvalidOptionalType(Expr<S, A> annot) {
		super(annot);
	}

	public Expr<S, A> getType() {
		return getAnnotation();
	}
	
}
