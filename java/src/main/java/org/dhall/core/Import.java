package org.dhall.core;

import org.dhall.core.imports.ImportHashed;
import org.dhall.core.imports.ImportMode;

public final class Import {
	private final ImportHashed hashed;
	private final ImportMode mode;
	public Import(ImportHashed hashed, ImportMode mode) {
		super();
		this.hashed = hashed;
		this.mode = mode;
	}
	public ImportHashed getHashed() {
		return hashed;
	}
	public ImportMode getMode() {
		return mode;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((hashed == null) ? 0 : hashed.hashCode());
		result = prime * result + ((mode == null) ? 0 : mode.hashCode());
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
		Import other = (Import) obj;
		if (hashed == null) {
			if (other.hashed != null)
				return false;
		} else if (!hashed.equals(other.hashed))
			return false;
		if (mode != other.mode)
			return false;
		return true;
	}
	@Override
	public String toString() {
		return "Import [hashed=" + hashed + ", mode=" + mode + "]";
	}
	
	
}
