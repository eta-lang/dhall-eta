package org.dhall.core.expr;

import java.util.List;
import java.util.Optional;

import org.dhall.core.Expr;

public final class ExprListLit<S,A> extends Expr<S, A> 
									implements Literal<List<Expr<S,A>>>,OptionallyTyped<S, A>{
	final Optional<Expr<S,A>> type;
	final List<Expr<S,A>> elems;
	
	
	public ExprListLit(Optional<Expr<S,A>> type, List<Expr<S,A>> elems) {
		super();
		this.type = type;
		this.elems = elems;
	}
	
	public Optional<Expr<S,A>> getType() {
		return type;
	}
	
	@Override
	public List<Expr<S,A>> getValue() {
		return elems;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((elems == null) ? 0 : elems.hashCode());
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
		ExprListLit<S,A> other = (ExprListLit<S,A>) obj;
		if (elems == null) {
			if (other.elems != null)
				return false;
		} else if (!elems.equals(other.elems))
			return false;
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	public String elemsToString() {
		String s="List [";
		for (Expr<S,A> e:elems) {
			s+=e.toString()+",";
		}
		int sepIdx = s.lastIndexOf(',');
		if (sepIdx >= 0)
			s=s.substring(0, sepIdx);
		s=s+"]";
		return s;
	}

	@Override
	public String toString() {
		return "ExprListLit [type=" + type + ", elems=" + elemsToString() + "]";
	}
		
	
}
