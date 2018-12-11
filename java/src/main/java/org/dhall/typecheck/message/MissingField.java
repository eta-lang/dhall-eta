package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class MissingField<S,A> extends TypeMessage<S,A> {
	
	private final String missingField;
	private final Expr<S,A> actualFields;
	
	public MissingField(String missingField, Expr<S, A> actualFields) {
		super();
		this.missingField = missingField;
		this.actualFields = actualFields;
	}

	public String getMissingField() {
		return missingField;
	}

	public Expr<S, A> getActualFields() {
		return actualFields;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((actualFields == null) ? 0 : actualFields.hashCode());
		result = prime * result + ((missingField == null) ? 0 : missingField.hashCode());
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
		MissingField<S,A> other = (MissingField<S,A>) obj;
		if (actualFields == null) {
			if (other.actualFields != null)
				return false;
		} else if (!actualFields.equals(other.actualFields))
			return false;
		if (missingField == null) {
			if (other.missingField != null)
				return false;
		} else if (!missingField.equals(other.missingField))
			return false;
		return true;
	}

}
