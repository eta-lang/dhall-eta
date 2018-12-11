package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public abstract class AnnotationMessage<S,A> extends TypeMessage<S,A> {
	
	private final Expr<S, A> annotation;

	public AnnotationMessage(Expr<S, A> annotation) {
		super();
		this.annotation = annotation;
	}

	public Expr<S, A> getAnnotation() {
		return annotation;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((annotation == null) ? 0 : annotation.hashCode());
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
		AnnotationMessage<S,A> other = (AnnotationMessage<S,A>) obj;
		if (annotation == null) {
			if (other.annotation != null)
				return false;
		} else if (!annotation.equals(other.annotation))
			return false;
		return true;
	}

	
}
