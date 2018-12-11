package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprEmbed<S, A> extends Expr<S, A> {
	private final A embedded;

	public ExprEmbed(A embedded) {
		super();
		this.embedded = embedded;
	}

	public A getEmbedded() {
		return embedded;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((embedded == null) ? 0 : embedded.hashCode());
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
		ExprEmbed<S, A> other = (ExprEmbed<S, A>) obj;
		if (embedded == null) {
			if (other.embedded != null)
				return false;
		} else if (!embedded.equals(other.embedded))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprEmbed [embedded=" + embedded + "]";
	}
	
	
	
}
