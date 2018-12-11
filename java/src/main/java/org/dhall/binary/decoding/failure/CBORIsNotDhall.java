/**
 * 
 */
package org.dhall.binary.decoding.failure;

import java.util.Arrays;

import org.dhall.binary.DecodingFailure;

/**
 * @author Javier Neira Sanchez
 *
 */
public class CBORIsNotDhall extends DecodingFailure {
	private final byte[] term;

	public CBORIsNotDhall(byte[] term) {
		super();
		this.term = term;
	}

	public byte[] getTerm() {
		return term;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + Arrays.hashCode(term);
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
		CBORIsNotDhall other = (CBORIsNotDhall) obj;
		if (!Arrays.equals(term, other.term))
			return false;
		return true;
	}
	
	
}
