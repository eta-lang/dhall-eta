package org.dhall.typecheck.message;

import org.dhall.Named;
import org.dhall.core.Expr;

public abstract class AnnotatedNameMessage<S,A> extends AnnotationMessage<S,A> implements Named{
    
	private final String name;
	
    public AnnotatedNameMessage(String name, Expr<S, A> annot) {
		super(annot);
		this.name = name;
	}

    public String getName() {
    	return name;
    }
    
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		AnnotatedNameMessage<S,A> other = (AnnotatedNameMessage<S,A>) obj;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		return true;
	}
	
    
}
