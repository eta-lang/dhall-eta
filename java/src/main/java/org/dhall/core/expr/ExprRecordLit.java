package org.dhall.core.expr;

import java.util.LinkedHashMap;

import org.dhall.core.Expr;

public final class ExprRecordLit<S,A> extends Expr<S, A> implements Literal<LinkedHashMap<String,Expr<S,A>>>{
	final LinkedHashMap<String,Expr<S,A>> value;

	public ExprRecordLit(LinkedHashMap<String, Expr<S, A>> value) {
		super();
		this.value = value;
	}

	public LinkedHashMap<String, Expr<S, A>> getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		ExprRecordLit<S,A> other = (ExprRecordLit<S,A>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprRecordLit [value=" + value + "]";
	}
	
	
	
		
}
