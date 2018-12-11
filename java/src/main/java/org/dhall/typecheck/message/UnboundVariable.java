package org.dhall.typecheck.message;

public final class UnboundVariable<S,A> extends NameMessage<S,A> {
    
	public UnboundVariable(String name) {
		super(name);
	}


}
