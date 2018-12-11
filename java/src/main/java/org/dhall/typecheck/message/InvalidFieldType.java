package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidFieldType<S,A> extends AnnotatedNameMessage<S,A>{
    
    public InvalidFieldType(String name, Expr<S, A> annot) {
		super(name,annot);
	}
	
}
