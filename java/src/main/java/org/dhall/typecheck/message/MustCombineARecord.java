package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class MustCombineARecord<S,A> extends AnnotatedExpressionMessage<S,A> {
	
	private final Character operation;
	
	public MustCombineARecord(Character operation, Expr<S, A> expression, Expr<S, A> annot) {
		super(expression,annot);
		this.operation = operation;
	}

	public Character getOperation() {
		return operation;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + ((operation == null) ? 0 : operation.hashCode());
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
		MustCombineARecord<S,A> other = (MustCombineARecord<S,A>) obj;
		if (operation == null) {
			if (other.operation != null)
				return false;
		} else if (!operation.equals(other.operation))
			return false;
		return true;
	}

}
