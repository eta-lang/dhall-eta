package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class IfBranchMustBeTerm<S,A> extends TypeMessage<S,A> {
    
	private final Boolean isThen;
	private final Expr<S,A> expression;
	private final Expr<S,A> kind;
	private final Expr<S,A> sort;
	
	public IfBranchMustBeTerm(Boolean isThen, Expr<S, A> expression, Expr<S, A> kind, Expr<S, A> sort) {
		super();
		this.isThen = isThen;
		this.expression = expression;
		this.kind = kind;
		this.sort = sort;
	}
	
	public Boolean getIsThen() {
		return isThen;
	}
	public Expr<S, A> getExpression() {
		return expression;
	}
	public Expr<S, A> getKind() {
		return kind;
	}
	public Expr<S, A> getSort() {
		return sort;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((expression == null) ? 0 : expression.hashCode());
		result = prime * result + ((isThen == null) ? 0 : isThen.hashCode());
		result = prime * result + ((kind == null) ? 0 : kind.hashCode());
		result = prime * result + ((sort == null) ? 0 : sort.hashCode());
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
		IfBranchMustBeTerm<S,A> other = (IfBranchMustBeTerm<S,A>) obj;
		if (expression == null) {
			if (other.expression != null)
				return false;
		} else if (!expression.equals(other.expression))
			return false;
		if (isThen == null) {
			if (other.isThen != null)
				return false;
		} else if (!isThen.equals(other.isThen))
			return false;
		if (kind == null) {
			if (other.kind != null)
				return false;
		} else if (!kind.equals(other.kind))
			return false;
		if (sort == null) {
			if (other.sort != null)
				return false;
		} else if (!sort.equals(other.sort))
			return false;
		return true;
	}
	
}
