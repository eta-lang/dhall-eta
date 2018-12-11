package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class TypeMismatch<S,A> extends AnnotatedExpressionMismatchMessage<S,A> {
	
	private final Expr<S,A> function;
	
	public TypeMismatch(Expr<S, A> function, Expr<S, A> expectedType, 
			Expr<S, A> argument, Expr<S, A> actualType) {
		super(argument,expectedType,actualType);
		this.function = function;
	}
	
	public Expr<S, A> getFunction() {
		return function;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((function == null) ? 0 : function.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		TypeMismatch<S,A> other = (TypeMismatch<S,A>) obj;
		if (function == null) {
			if (other.function != null)
				return false;
		} else if (!function.equals(other.function))
			return false;
		return true;
	}
	
	
}
