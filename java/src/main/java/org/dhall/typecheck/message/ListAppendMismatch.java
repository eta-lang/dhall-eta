package org.dhall.typecheck.message;

import org.dhall.core.Expr;
import org.dhall.typecheck.TypeMessage;

public final class ListAppendMismatch<S,A> extends TypeMessage<S,A> {

	private final Expr<S,A> list1Type;
	private final Expr<S,A> list2Type;
	
	public ListAppendMismatch(Expr<S, A> list1Type, Expr<S, A> list2Type) {
		super();
		this.list1Type = list1Type;
		this.list2Type = list2Type;
	}

	public Expr<S, A> getList1Type() {
		return list1Type;
	}

	public Expr<S, A> getList2Type() {
		return list2Type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((list1Type == null) ? 0 : list1Type.hashCode());
		result = prime * result + ((list2Type == null) ? 0 : list2Type.hashCode());
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
		ListAppendMismatch<S,A> other = (ListAppendMismatch<S,A>) obj;
		if (list1Type == null) {
			if (other.list1Type != null)
				return false;
		} else if (!list1Type.equals(other.list1Type))
			return false;
		if (list2Type == null) {
			if (other.list2Type != null)
				return false;
		} else if (!list2Type.equals(other.list2Type))
			return false;
		return true;
	}

}
