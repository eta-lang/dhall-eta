package org.dhall.core.imports.types;

import org.dhall.core.imports.File;
import org.dhall.core.imports.FilePrefix;
import org.dhall.core.imports.ImportType;

public final class Local extends ImportType {
	private final FilePrefix filePrefix;
	private final File file;
	public Local(FilePrefix filePrefix, File file) {
		super();
		this.filePrefix = filePrefix;
		this.file = file;
	}
	public FilePrefix getFilePrefix() {
		return filePrefix;
	}
	public File getFile() {
		return file;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((file == null) ? 0 : file.hashCode());
		result = prime * result + ((filePrefix == null) ? 0 : filePrefix.hashCode());
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
		Local other = (Local) obj;
		if (file == null) {
			if (other.file != null)
				return false;
		} else if (!file.equals(other.file))
			return false;
		if (filePrefix != other.filePrefix)
			return false;
		return true;
	}
	
	
}
