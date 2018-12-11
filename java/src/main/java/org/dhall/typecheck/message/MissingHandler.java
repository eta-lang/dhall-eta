package org.dhall.typecheck.message;

import java.util.Set;

import org.dhall.typecheck.TypeMessage;

public final class MissingHandler<S,A> extends TypeMessage<S,A> {
	
	private final Set<String> handlers;

	public MissingHandler(Set<String> handlers) {
		super();
		this.handlers = handlers;
	}

	public Set<String> getHandlers() {
		return handlers;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((handlers == null) ? 0 : handlers.hashCode());
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
		MissingHandler<S,A> other = (MissingHandler<S,A>) obj;
		if (handlers == null) {
			if (other.handlers != null)
				return false;
		} else if (!handlers.equals(other.handlers))
			return false;
		return true;
	}
	
	
}
