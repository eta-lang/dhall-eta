package org.dhall.typecheck.message;

import org.dhall.typecheck.TypeMessage;

public final class MissingListType<S,A> extends TypeMessage<S,A> {

	public MissingListType() {
		super();
	}

	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}

    
}
