package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidField<S,A> extends AnnotatedNameMessage<S,A>{
    
    public InvalidField(String name, Expr<S, A> annot) {
		super(name, annot);
	}
	
}
