package org.dhall.core.expr;

import org.dhall.core.Expr;

public abstract class ExprBinary<S, A> extends Expr<S, A> {

	private final Expr<S,A> left;
	private final Expr<S,A> right;
	
	public ExprBinary(Expr<S, A> left, Expr<S, A> right) {
		super();
		this.left = left;
		this.right = right;
	}

	public Expr<S, A> getLeft() {
		return left;
	}

	public Expr<S, A> getRight() {
		return right;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((right == null) ? 0 : right.hashCode());
		result = prime * result + ((left == null) ? 0 : left.hashCode());
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
		ExprBinary<S, A> other = (ExprBinary<S, A>) obj;
		if (right == null) {
			if (other.right != null)
				return false;
		} else if (!right.equals(other.right))
			return false;
		if (left == null) {
			if (other.left != null)
				return false;
		} else if (!left.equals(other.left))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return getClass().getSimpleName()+" ["+getLeftLabel()+"=" + left +
				", "+getRightLabel()+"=" + right + "]";
	}
	
	protected String getLeftLabel() {
		return "left";
	}
	protected String getRightLabel() {
		return "right";
	}
	
}	
