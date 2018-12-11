package org.dhall.core.expr;

import java.util.LinkedHashMap;

import org.dhall.core.Expr;

public final class ExprUnion<S,A> extends Expr<S, A> {
	final LinkedHashMap<String,Expr<S,A>> type;

	public ExprUnion(LinkedHashMap<String, Expr<S, A>> type) {
		super();
		this.type = type;
	}

	public LinkedHashMap<String, Expr<S, A>> getType() {
		return type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((type == null) ? 0 : type.hashCode());
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
		ExprUnion<S,A> other = (ExprUnion<S,A>) obj;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprUnion [type=" + type + "]";
	}
	
		
}
