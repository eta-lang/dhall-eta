package org.dhall.core.expr;

import org.dhall.Named;
import org.dhall.core.Expr;

public final class ExprPi<S, A> extends Expr<S, A> implements Named {
	private final String name;
	private final Expr<S,A> kind;
	private final Expr<S,A> body;
	
	public ExprPi(String name, Expr<S, A> kind, Expr<S, A> body) {
		super();
		this.name = name;
		this.kind = kind;
		this.body = body;
	}

	@Override
	public String getName() {
		return name;
	}

	public Expr<S, A> getKind() {
		return kind;
	}

	public Expr<S, A> getBody() {
		return body;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((body == null) ? 0 : body.hashCode());
		result = prime * result + ((name == null) ? 0 : name.hashCode());
		result = prime * result + ((kind == null) ? 0 : kind.hashCode());
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
		ExprPi<S, A> other = (ExprPi<S, A>) obj;
		if (body == null) {
			if (other.body != null)
				return false;
		} else if (!body.equals(other.body))
			return false;
		if (name == null) {
			if (other.name != null)
				return false;
		} else if (!name.equals(other.name))
			return false;
		if (kind == null) {
			if (other.kind != null)
				return false;
		} else if (!kind.equals(other.kind))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprPi [name=" + name + ", kind=" + kind + ", body=" + body + "]";
	}
	
	
}	
