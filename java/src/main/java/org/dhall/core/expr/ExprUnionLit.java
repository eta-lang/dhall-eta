package org.dhall.core.expr;

import java.util.LinkedHashMap;

import org.dhall.Named;
import org.dhall.core.Expr;

public final class ExprUnionLit<S,A> extends Expr<S, A> implements Literal<Expr<S,A>>, Named{
	private final String name;
	private final Expr<S,A> value;
	private final LinkedHashMap<String,Expr<S,A>> cases;
	
	public ExprUnionLit(String name, Expr<S, A> value, LinkedHashMap<String, Expr<S, A>> cases) {
		super();
		this.name = name;
		this.value = value;
		this.cases = cases;
	}

	public String getName() {
		return name;
	}

	public Expr<S, A> getValue() {
		return value;
	}

	public LinkedHashMap<String, Expr<S, A>> getCases() {
		return cases;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((cases == null) ? 0 : cases.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
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
		ExprUnionLit<S,A> other = (ExprUnionLit<S,A>) obj;
		if (cases == null) {
			if (other.cases != null)
				return false;
		} else if (!cases.equals(other.cases))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprUnionLit [name=" + name + ", value=" + value + ", cases=" + cases + "]";
	}
	
	
		
}
