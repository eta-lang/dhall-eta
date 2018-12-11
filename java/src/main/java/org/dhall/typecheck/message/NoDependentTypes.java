package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class NoDependentTypes<S,A> extends TypeMessage<S,A> {

	private final Expr<S,A> inputType;
	private final Expr<S,A> outputKind;
	
	public NoDependentTypes(Expr<S, A> list1Type, Expr<S, A> list2Type) {
		super();
		this.inputType = list1Type;
		this.outputKind = list2Type;
	}

	public Expr<S, A> getInputType() {
		return inputType;
	}

	public Expr<S, A> getOutputKind() {
		return outputKind;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((inputType == null) ? 0 : inputType.hashCode());
		result = prime * result + ((outputKind == null) ? 0 : outputKind.hashCode());
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
		NoDependentTypes<S,A> other = (NoDependentTypes<S,A>) obj;
		if (inputType == null) {
			if (other.inputType != null)
				return false;
		} else if (!inputType.equals(other.inputType))
			return false;
		if (outputKind == null) {
			if (other.outputKind != null)
				return false;
		} else if (!outputKind.equals(other.outputKind))
			return false;
		return true;
	}


}
