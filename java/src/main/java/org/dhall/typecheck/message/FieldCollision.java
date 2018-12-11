package org.dhall.typecheck.message;

public final class FieldCollision<S,A> extends NameMessage<S,A>{
    
	public FieldCollision(String name) {
		super(name);
	}


}
