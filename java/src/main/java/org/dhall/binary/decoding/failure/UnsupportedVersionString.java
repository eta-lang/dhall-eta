/**
 * 
 */
package org.dhall.binary.decoding.failure;

import org.dhall.binary.DecodingFailure;

/**
 * @author Javier Neira Sanchez
 *
 */
public class UnsupportedVersionString extends DecodingFailure {
	private final String versionString;

	public UnsupportedVersionString(String versionString) {
		super();
		this.versionString = versionString;
	}

	public String getVersionString() {
		return versionString;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((versionString == null) ? 0 : versionString.hashCode());
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
		UnsupportedVersionString other = (UnsupportedVersionString) obj;
		if (versionString == null) {
			if (other.versionString != null)
				return false;
		} else if (!versionString.equals(other.versionString))
			return false;
		return true;
	}
	
	
}
