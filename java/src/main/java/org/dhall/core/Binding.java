package org.dhall.core;

import java.util.Optional;

public class Binding<S,A> {
    
    private final String variable;
    private final Optional<Expr<S,A>> annotation;
    private final Expr<S,A> value;
	public Binding(String variable, Optional<Expr<S, A>> annotation, Expr<S, A> value) {
		super();
		this.variable = variable;
		this.annotation = annotation;
		this.value = value;
	}
	public String getVariable() {
		return variable;
	}
	public Optional<Expr<S, A>> getAnnotation() {
		return annotation;
	}
	public Expr<S, A> getValue() {
		return value;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((annotation == null) ? 0 : annotation.hashCode());
		result = prime * result + ((value == null) ? 0 : value.hashCode());
		result = prime * result + ((variable == null) ? 0 : variable.hashCode());
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
		@SuppressWarnings("rawtypes")
		Binding other = (Binding) obj;
		if (annotation == null) {
			if (other.annotation != null)
				return false;
		} else if (!annotation.equals(other.annotation))
			return false;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		if (variable == null) {
			if (other.variable != null)
				return false;
		} else if (!variable.equals(other.variable))
			return false;
		return true;
	}
	@Override
	public String toString() {
		return "Binding [variable=" + variable + ", annotation=" + annotation + ", value=" + value + "]";
	}

    
    
}
