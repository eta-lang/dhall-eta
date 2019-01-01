package org.dhall.typecheck.message;

import org.dhall.core.Const;
import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class RecordMismatch<S,A> extends TypeMessage<S,A> {
	
	private final Character operation;
	private final Const record1Const;
	private final Const record2Const;
	private final Expr<S,A> record1;
	private final Expr<S,A> record2;
	
	public RecordMismatch(int operation, 
			Expr<S, A> record1, Expr<S, A> record2,
			Const record1Const,  Const record2Const) {
		this((char)operation,record1,record2,record1Const,record2Const);
	}
	
	public RecordMismatch(char operation, 
			Expr<S, A> record1, Expr<S, A> record2,
			Const record1Const,  Const record2Const) {
		this(Character.valueOf(operation),record1,record2,record1Const,record2Const);
	}
	
	public RecordMismatch(Character operation, 
			Expr<S, A> record1, Expr<S, A> record2,
			Const record1Const,  Const record2Const) {
		super();
		this.operation = operation;
		this.record1 = record1;
		this.record1Const = record1Const;
		this.record2 = record2;
		this.record2Const = record2Const;
	}

	public Character getOperation() {
		return operation;
	}

	public Const getRecord1Const() {
		return record1Const;
	}

	public Const getRecord2Const() {
		return record2Const;
	}

	public Expr<S, A> getRecord1() {
		return record1;
	}

	public Expr<S, A> getRecord2() {
		return record2;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((operation == null) ? 0 : operation.hashCode());
		result = prime * result + ((record1 == null) ? 0 : record1.hashCode());
		result = prime * result + ((record1Const == null) ? 0 : record1Const.hashCode());
		result = prime * result + ((record2 == null) ? 0 : record2.hashCode());
		result = prime * result + ((record2Const == null) ? 0 : record2Const.hashCode());
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
		RecordMismatch<S,A> other = (RecordMismatch<S,A>) obj;
		if (operation == null) {
			if (other.operation != null)
				return false;
		} else if (!operation.equals(other.operation))
			return false;
		if (record1 == null) {
			if (other.record1 != null)
				return false;
		} else if (!record1.equals(other.record1))
			return false;
		if (record1Const == null) {
			if (other.record1Const != null)
				return false;
		} else if (!record1Const.equals(other.record1Const))
			return false;
		if (record2 == null) {
			if (other.record2 != null)
				return false;
		} else if (!record2.equals(other.record2))
			return false;
		if (record2Const == null) {
			if (other.record2Const != null)
				return false;
		} else if (!record2Const.equals(other.record2Const))
			return false;
		return true;
	}


	
}
