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
public final class CannotDecodeVersionString extends DecodingFailure {
	private  byte[] term;

	public CannotDecodeVersionString(byte[] term) {
		super();
		this.term = term;
	}

	public byte[] getTerm() {
		return term;
	}

	public void setTerm(byte[] term) {
		this.term = term;
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
		CannotDecodeVersionString other = (CannotDecodeVersionString) obj;
		if (!Arrays.equals(term, other.term))
			return false;
		return true;
	}
	
	
	
}
