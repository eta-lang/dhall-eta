package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class IfBranchMismatch<S,A> extends TypeMessage<S,A> {

	private final Expr<S,A> thenExpression;
	private final Expr<S,A> thenType;
	private final Expr<S,A> elseExpression;
	private final Expr<S,A> elseType;
	
	public IfBranchMismatch(Expr<S, A> thenExpression, Expr<S, A> thenType, 
			Expr<S, A> elseExpression, Expr<S, A> elseType) {
		super();
		this.thenExpression = thenExpression;
		this.thenType = thenType;
		this.elseExpression = elseExpression;
		this.elseType = elseType;
	}
	
	public Expr<S, A> getThenExpression() {
		return thenExpression;
	}
	public Expr<S, A> getThenType() {
		return thenType;
	}
	public Expr<S, A> getElseExpression() {
		return elseExpression;
	}
	public Expr<S, A> getElseType() {
		return elseType;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((elseExpression == null) ? 0 : elseExpression.hashCode());
		result = prime * result + ((elseType == null) ? 0 : elseType.hashCode());
		result = prime * result + ((thenExpression == null) ? 0 : thenExpression.hashCode());
		result = prime * result + ((thenType == null) ? 0 : thenType.hashCode());
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
		IfBranchMismatch<S,A> other = (IfBranchMismatch<S,A>) obj;
		if (elseExpression == null) {
			if (other.elseExpression != null)
				return false;
		} else if (!elseExpression.equals(other.elseExpression))
			return false;
		if (elseType == null) {
			if (other.elseType != null)
				return false;
		} else if (!elseType.equals(other.elseType))
			return false;
		if (thenExpression == null) {
			if (other.thenExpression != null)
				return false;
		} else if (!thenExpression.equals(other.thenExpression))
			return false;
		if (thenType == null) {
			if (other.thenType != null)
				return false;
		} else if (!thenType.equals(other.thenType))
			return false;
		return true;
	}
	
	
}
