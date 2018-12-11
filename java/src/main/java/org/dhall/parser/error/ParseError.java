package org.dhall.parser.error;

public class ParseError {
	private final String message;
	private final String input;
	public ParseError(String message, String input) {
		super();
		this.message = message;
		this.input = input;
	}
	public String getMessage() {
		return message;
	}
	public String getInput() {
		return input;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((input == null) ? 0 : input.hashCode());
		result = prime * result + ((message == null) ? 0 : message.hashCode());
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
		ParseError other = (ParseError) obj;
		if (input == null) {
			if (other.input != null)
				return false;
		} else if (!input.equals(other.input))
			return false;
		if (message == null) {
			if (other.message != null)
				return false;
		} else if (!message.equals(other.message))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "Parse error:" + message;
	}
}
