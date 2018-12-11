package org.dhall.core.expr;

import org.dhall.core.Chunks;
import org.dhall.core.Expr;

public final class ExprTextLit<S,A> extends Expr<S, A> implements Literal<Chunks<S, A>>{
	final Chunks<S, A> chunks;

	public ExprTextLit(Chunks<S, A> chunks) {
		super();
		this.chunks = chunks;
	}
	
	@Override
	public Chunks<S, A> getValue() {
		return chunks;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((getValue() == null) ? 0 : getValue().hashCode());
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
		ExprTextLit<S,A> other = (ExprTextLit<S,A>) obj;
		if (getValue() == null) {
			if (other.getValue() != null)
				return false;
		} else if (!getValue().equals(other.getValue()))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprTextLit [chunks=" + chunks + "]";
	}

	
}
