package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprField<S,A> extends Expr<S, A> {
	private final String key;
	private final Expr<S,A> record;
	public ExprField(Expr<S, A> record, String key) {
		super();
		this.key = key;
		this.record = record;
	}
	public String getKey() {
		return key;
	}
	public Expr<S, A> getRecord() {
		return record;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((key == null) ? 0 : key.hashCode());
		result = prime * result + ((record == null) ? 0 : record.hashCode());
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
		ExprField<S,A> other = (ExprField<S,A>) obj;
		if (key == null) {
			if (other.key != null)
				return false;
		} else if (!key.equals(other.key))
			return false;
		if (record == null) {
			if (other.record != null)
				return false;
		} else if (!record.equals(other.record))
			return false;
		return true;
	}
	@Override
	public String toString() {
		return "ExprField [key=" + key + ", record=" + record + "]";
	}
	
		
		
}
