package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBoolIf<S, A> extends Expr<S, A> {

	private final Expr<S,A> test;
	private final Expr<S,A> then_;
	private final Expr<S,A> else_;
	
	public ExprBoolIf(Expr<S,A> test, Expr<S,A> then_, Expr<S, A> else_) {
		super();
		this.test = test;
		this.then_ = then_;
		this.else_ = else_;
	}

	public Expr<S, A> getTest() {
		return test;
	}

	public Expr<S, A> getThen() {
		return then_;
	}

	public Expr<S, A> getElse() {
		return else_;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((else_ == null) ? 0 : else_.hashCode());
		result = prime * result + ((test == null) ? 0 : test.hashCode());
		result = prime * result + ((then_ == null) ? 0 : then_.hashCode());
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
		ExprBoolIf<S, A> other = (ExprBoolIf<S, A>) obj;
		if (else_ == null) {
			if (other.else_ != null)
				return false;
		} else if (!else_.equals(other.else_))
			return false;
		if (test == null) {
			if (other.test != null)
				return false;
		} else if (!test.equals(other.test))
			return false;
		if (then_ == null) {
			if (other.then_ != null)
				return false;
		} else if (!then_.equals(other.then_))
			return false;
		return true;
	}
	
	
	
}	
