package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidAlternativeType<S,A> extends AnnotatedNameMessage<S,A>{
    
    public InvalidAlternativeType(String name, Expr<S, A> annot) {
		super(name,annot);
	}
	
}
