package org.dhall.typecheck.message;

import org.dhall.core.Const;
import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class FieldMismatch<S,A> extends TypeMessage<S,A> {
    
	private final String field1Name;
    private final Expr<S,A> field1Value;
    private final Const field1Constant;
    private final String field2Name;
    private final Expr<S,A> field2Value;
    private final Const field2Constant;
	
    public FieldMismatch(String field1Name, Expr<S, A> field1Value, Const field1Constant, String field2Name,
			Expr<S, A> field2Value, Const field2Constant) {
		super();
		this.field1Name = field1Name;
		this.field1Value = field1Value;
		this.field1Constant = field1Constant;
		this.field2Name = field2Name;
		this.field2Value = field2Value;
		this.field2Constant = field2Constant;
	}

	public String getField1Name() {
		return field1Name;
	}

	public Expr<S, A> getField1Value() {
		return field1Value;
	}

	public Const getField1Constant() {
		return field1Constant;
	}

	public String getField2Name() {
		return field2Name;
	}

	public Expr<S, A> getField2Value() {
		return field2Value;
	}

	public Const getField2Constant() {
		return field2Constant;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((field1Constant == null) ? 0 : field1Constant.hashCode());
		result = prime * result + ((field1Name == null) ? 0 : field1Name.hashCode());
		result = prime * result + ((field1Value == null) ? 0 : field1Value.hashCode());
		result = prime * result + ((field2Constant == null) ? 0 : field2Constant.hashCode());
		result = prime * result + ((field2Name == null) ? 0 : field2Name.hashCode());
		result = prime * result + ((field2Value == null) ? 0 : field2Value.hashCode());
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
		FieldMismatch<S,A> other = (FieldMismatch<S,A>) obj;
		if (field1Constant == null) {
			if (other.field1Constant != null)
				return false;
		} else if (!field1Constant.equals(other.field1Constant))
			return false;
		if (field1Name == null) {
			if (other.field1Name != null)
				return false;
		} else if (!field1Name.equals(other.field1Name))
			return false;
		if (field1Value == null) {
			if (other.field1Value != null)
				return false;
		} else if (!field1Value.equals(other.field1Value))
			return false;
		if (field2Constant == null) {
			if (other.field2Constant != null)
				return false;
		} else if (!field2Constant.equals(other.field2Constant))
			return false;
		if (field2Name == null) {
			if (other.field2Name != null)
				return false;
		} else if (!field2Name.equals(other.field2Name))
			return false;
		if (field2Value == null) {
			if (other.field2Value != null)
				return false;
		} else if (!field2Value.equals(other.field2Value))
			return false;
		return true;
	}
	
    
}
