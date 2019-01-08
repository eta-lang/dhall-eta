/**
 * 
 */
package org.dhall.common.types.functor;

/**
 * @author Javier Neira Sanchez
 *
 */
public class Identity<A> {
	private final A value;

	public Identity(A a) {
		super();
		this.value = a;
	}

	public A get() {
		return value;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((value == null) ? 0 : value.hashCode());
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
		Identity<A> other = (Identity<A>) obj;
		if (value == null) {
			if (other.value != null)
				return false;
		} else if (!value.equals(other.value))
			return false;
		return true;
	}

	@Override
	public String toString() {
		return "Identity [value=" + value + "]";
	}
	
	
}
