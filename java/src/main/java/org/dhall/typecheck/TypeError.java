package org.dhall.typecheck;

import org.dhall.Context;
import org.dhall.core.Expr;

public class TypeError<S, A> {
	private final Context<A> context;
	private final Expr<S, A> current;
	private final TypeMessage<S, A> message;
	public TypeError(Context<A> context, Expr<S, A> current, TypeMessage<S, A> message) {
		super();
		this.context = context;
		this.current = current;
		this.message = message;
	}
	public Context<A> getContext() {
		return context;
	}
	public Expr<S, A> getCurrent() {
		return current;
	}
	public TypeMessage<S, A> getMessage() {
		return message;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((context == null) ? 0 : context.hashCode());
		result = prime * result + ((current == null) ? 0 : current.hashCode());
		result = prime * result + ((message == null) ? 0 : message.hashCode());
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
		TypeError<S,A> other = (TypeError<S,A>) obj;
		if (context == null) {
			if (other.context != null)
				return false;
		} else if (!context.equals(other.context))
			return false;
		if (current == null) {
			if (other.current != null)
				return false;
		} else if (!current.equals(other.current))
			return false;
		if (message == null) {
			if (other.message != null)
				return false;
		} else if (!message.equals(other.message))
			return false;
		return true;
	}
	
	
}
