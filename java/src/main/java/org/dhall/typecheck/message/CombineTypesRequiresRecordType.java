package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class CombineTypesRequiresRecordType<S,A> extends TypeMessage<S,A> {

	private final Expr<S,A> argument;
	private final Expr<S,A> normalized;
	
	public CombineTypesRequiresRecordType(Expr<S, A> argument, Expr<S, A> normalized) {
		super();
		this.argument = argument;
		this.normalized = normalized;
	}

	public Expr<S, A> getArgument() {
		return argument;
	}

	public Expr<S, A> getNormalized() {
		return normalized;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((argument == null) ? 0 : argument.hashCode());
		result = prime * result + ((normalized == null) ? 0 : normalized.hashCode());
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
		CombineTypesRequiresRecordType<S,A> other = (CombineTypesRequiresRecordType<S,A>) obj;
		if (argument == null) {
			if (other.argument != null)
				return false;
		} else if (!argument.equals(other.argument))
			return false;
		if (normalized == null) {
			if (other.normalized != null)
				return false;
		} else if (!normalized.equals(other.normalized))
			return false;
		return true;
	}

}
