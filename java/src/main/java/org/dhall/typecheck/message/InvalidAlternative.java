package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class InvalidAlternative<S,A> extends AnnotatedNameMessage<S,A>{
    
    public InvalidAlternative(String name, Expr<S, A> annot) {
		super(name,annot);
	}
	
}
