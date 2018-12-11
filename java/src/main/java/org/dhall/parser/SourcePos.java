package org.dhall.parser;

import java.nio.file.FileSystems;
import java.nio.file.Path;

public class SourcePos {
	private final Path path;
	private final Pos line;
	private final Pos column;
	
	public SourcePos(Path path, Pos line, Pos column) {
		super();
		this.path = path;
		this.line = line;
		this.column = column;
	}

	public SourcePos(String name, Pos line, Pos column) {
		this(FileSystems.getDefault().getPath(name),line,column);
	}
	
	public Path getPath() {
		return path;
	}

	public String getFileName() {
		return getPath().toString();
	}
	
	public Pos getLine() {
		return line;
	}

	public Pos getColumn() {
		return column;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((column == null) ? 0 : column.hashCode());
		result = prime * result + ((line == null) ? 0 : line.hashCode());
		result = prime * result + ((path == null) ? 0 : path.hashCode());
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
		SourcePos other = (SourcePos) obj;
		if (column == null) {
			if (other.column != null)
				return false;
		} else if (!column.equals(other.column))
			return false;
		if (line == null) {
			if (other.line != null)
				return false;
		} else if (!line.equals(other.line))
			return false;
		if (path == null) {
			if (other.path != null)
				return false;
		} else if (!path.equals(other.path))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "SourcePos{path="+path+",line="+line+",column="+column+"}";
	}
}
