package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public abstract class AnnotatedExpressionMessage<S,A> extends AnnotationMessage<S,A> {

	private final Expr<S,A> expression;
	
	public AnnotatedExpressionMessage(Expr<S, A> expression, Expr<S, A> annot) {
		super(annot);
		this.expression = expression;
	}

	public Expr<S, A> getExpression() {
		return expression;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((expression == null) ? 0 : expression.hashCode());
		return result;
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		@SuppressWarnings("unchecked")
		AnnotatedExpressionMessage<S,A> other = (AnnotatedExpressionMessage<S,A>) obj;
		if (expression == null) {
			if (other.expression != null)
				return false;
		} else if (!expression.equals(other.expression))
			return false;
		return true;
	}

	
}
