package org.dhall.core.expr;

import java.util.Optional;

import org.dhall.core.Expr;

public final class ExprOptionalLit<S,A> extends Expr<S, A> 
                                        implements Literal<Optional<Expr<S,A>>>,Typed<S, A>{
	final Expr<S,A> type;
	final Optional<Expr<S,A>> value;
	
	
	public ExprOptionalLit(Expr<S,A> type, Optional<Expr<S,A>> value) {
		super();
		this.type = type;
		this.value = value;
	}
	
	public Expr<S,A> getType() {
		return type;
	}
	
	@Override
	public Optional<Expr<S,A>> getValue() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		ExprOptionalLit<S,A> other = (ExprOptionalLit<S,A>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprOptionalLit [type=" + type + ", value=" + value + "]";
	}
	
}
