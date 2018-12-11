package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class HandlerNotAFunction<S,A> extends AnnotatedNameMessage<S,A>{
    
    public HandlerNotAFunction(String name, Expr<S, A> annot) {
		super(name, annot);
	}
	
	public String getHandler() {
		return getName();
	}


}
