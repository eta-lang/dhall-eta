package org.dhall.core.expr;

import java.util.Set;

import org.dhall.core.Expr;

public final class ExprProject<S,A> extends Expr<S, A> {
	private final Set<String> keys;
	private final Expr<S,A> record;
	public ExprProject(Expr<S, A> record, Set<String> keys) {
		super();
		this.record = record;
		this.keys = keys;
	}
	public Set<String> getKeys() {
		return keys;
	}
	public Expr<S, A> getRecord() {
		return record;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((keys == null) ? 0 : keys.hashCode());
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
		ExprProject<S, A> other = (ExprProject<S, A>) obj;
		if (keys == null) {
			if (other.keys != null)
				return false;
		} else if (!keys.equals(other.keys))
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
		return "ExprProject [keys=" + keys + ", record=" + record + "]";
	}
		
	
	
}
