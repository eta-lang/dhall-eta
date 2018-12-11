package org.dhall.core.imports;

import java.util.Optional;

import org.dhall.core.imports.hashed.Digest;
import org.dhall.core.imports.hashed.SHA256;

public class ImportHashed {
	private final Optional<Digest<SHA256>> digest;
	private final ImportType type;
	
	public ImportHashed(Optional<Digest<SHA256>> digest, ImportType type) {
		super();
		this.digest = digest;
		this.type = type;
	}

	public Optional<Digest<SHA256>> getDigest() {
		return digest;
	}

	public ImportType getType() {
		return type;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((digest == null) ? 0 : digest.hashCode());
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
		ImportHashed other = (ImportHashed) obj;
		if (digest == null) {
			if (other.digest != null)
				return false;
		} else if (!digest.equals(other.digest))
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
		return "ImportHashed [digest=" + digest + ", type=" + type + "]";
	}
	
	
}
