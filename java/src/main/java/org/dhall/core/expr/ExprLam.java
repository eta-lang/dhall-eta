package org.dhall.core.expr;

import org.dhall.Named;
import org.dhall.core.Expr;

public final class ExprLam<S, A> extends Expr<S, A>  implements Named,Typed<S, A> {
	private final String name;
	private final Expr<S,A> type;
	private final Expr<S,A> body;
	
	public ExprLam(String name, Expr<S, A> type, Expr<S, A> body) {
		super();
		this.name = name;
		this.type = type;
		this.body = body;
	}
	
    @Override
	public String getName() {
		return name;
	}

	public Expr<S, A> getType() {
		return type;
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
		result = prime * result + ((type == null) ? 0 : type.hashCode());
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
		ExprLam<S, A> other = (ExprLam<S, A>) obj;
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
		if (type == null) {
			if (other.type != null)
				return false;
		} else if (!type.equals(other.type))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "ExprLam [name=" + name + ", type=" + type + ", body=" + body + "]";
	}
	
	
}	
