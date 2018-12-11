package org.dhall.parser;

public class Src {
	private final SourcePos begin;
	private final SourcePos end;
	private final String text;
	
	public Src(SourcePos begin, SourcePos end, String text) {
		super();
		this.begin = begin;
		this.end = end;
		this.text = text;
	}

	public SourcePos getBegin() {
		return begin;
	}

	public SourcePos getEnd() {
		return end;
	}

	public String getText() {
		return text;
	}

	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((end == null) ? 0 : end.hashCode());
		result = prime * result + ((begin == null) ? 0 : begin.hashCode());
		result = prime * result + ((text == null) ? 0 : text.hashCode());
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
		Src other = (Src) obj;
		if (end == null) {
			if (other.end != null)
				return false;
		} else if (!end.equals(other.end))
			return false;
		if (begin == null) {
			if (other.begin != null)
				return false;
		} else if (!begin.equals(other.begin))
			return false;
		if (text == null) {
			if (other.text != null)
				return false;
		} else if (!text.equals(other.text))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "Src{begin="+begin+",end="+end+",text=\""+text+"\"}";
	}
}
