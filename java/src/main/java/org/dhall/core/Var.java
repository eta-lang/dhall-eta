package org.dhall.core;

import org.haskell.types.Pair;

public final class Var extends Pair<String, Integer>{

    public Var(String name, Integer index) {
       super(name, index);
    }
    
    public String getName() {
		return getFst();
	}

	public Integer getIndex() {
		return getSnd();
	}
    
    @Override
	public String toString() {
		return "Var [name=" + getName() + ", index=" + getIndex() + "]";
	}

    
}
