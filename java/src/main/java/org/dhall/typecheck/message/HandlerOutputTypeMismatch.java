package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class HandlerOutputTypeMismatch<S,A> extends TypeMessage<S,A> {
    
	private final String alternative1;
    private final Expr<S,A> outputType1;
    private final String alternative2;
    private final Expr<S,A> outputType2;
	
    public HandlerOutputTypeMismatch(String field1Name, Expr<S, A> field1Annotation, String field2Name,
			Expr<S, A> field2Annotation) {
		super();
		this.alternative1 = field1Name;
		this.outputType1 = field1Annotation;
		this.alternative2 = field2Name;
		this.outputType2 = field2Annotation;
	}

	public String getAlternative1() {
		return alternative1;
	}

	public Expr<S, A> getOutputType1() {
		return outputType1;
	}

	public String getAlternative2() {
		return alternative2;
	}

	public Expr<S, A> getOutputType2() {
		return outputType2;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((alternative1 == null) ? 0 : alternative1.hashCode());
		result = prime * result + ((alternative2 == null) ? 0 : alternative2.hashCode());
		result = prime * result + ((outputType1 == null) ? 0 : outputType1.hashCode());
		result = prime * result + ((outputType2 == null) ? 0 : outputType2.hashCode());
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
		HandlerOutputTypeMismatch<S,A> other = (HandlerOutputTypeMismatch<S,A>) obj;
		if (alternative1 == null) {
			if (other.alternative1 != null)
				return false;
		} else if (!alternative1.equals(other.alternative1))
			return false;
		if (alternative2 == null) {
			if (other.alternative2 != null)
				return false;
		} else if (!alternative2.equals(other.alternative2))
			return false;
		if (outputType1 == null) {
			if (other.outputType1 != null)
				return false;
		} else if (!outputType1.equals(other.outputType1))
			return false;
		if (outputType2 == null) {
			if (other.outputType2 != null)
				return false;
		} else if (!outputType2.equals(other.outputType2))
			return false;
		return true;
	}
	
    
    
}
