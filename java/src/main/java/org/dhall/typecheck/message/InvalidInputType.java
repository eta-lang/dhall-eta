package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidInputType<S,A> extends AnnotationMessage<S,A> {
	
	public InvalidInputType(Expr<S, A> annot) {
		super(annot);
	}

}
