package org.dhall.imports;

import java.util.Map;

import org.dhall.Context;
import org.dhall.StandardVersion;
import org.dhall.core.Expr;
import org.dhall.core.Import;
import org.dhall.parser.Src;
import org.haskell.types.NonEmptyArrayList;

public class Status {
	private final NonEmptyArrayList<Import> stack;
	private final Map<Import,Expr<Src, Void>> cache;
	private final StandardVersion standardVersion;
	private final Context<Expr<Src,Void>> startingContext;
	
	public Status(NonEmptyArrayList<Import> stack, Map<Import, Expr<Src, Void>> cache, 
			StandardVersion standardVersion,Context<Expr<Src, Void>> initialContext) {
		super();
		this.stack = stack;
		this.cache = cache;
		this.standardVersion = standardVersion;
		this.startingContext = initialContext;
	}

	public NonEmptyArrayList<Import> getStack() {
		return stack;
	}

	public Map<Import, Expr<Src, Void>> getCache() {
		return cache;
	}

	public StandardVersion getStandardVersion() {
		return standardVersion;
	}

	public Context<Expr<Src, Void>> getStartingContext() {
		return startingContext;
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((cache == null) ? 0 : cache.hashCode());
		result = prime * result + ((startingContext == null) ? 0 : startingContext.hashCode());
		result = prime * result + ((stack == null) ? 0 : stack.hashCode());
		result = prime * result + ((standardVersion == null) ? 0 : standardVersion.hashCode());
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
		Status other = (Status) obj;
		if (cache == null) {
			if (other.cache != null)
				return false;
		} else if (!cache.equals(other.cache))
			return false;
		if (startingContext == null) {
			if (other.startingContext != null)
				return false;
		} else if (!startingContext.equals(other.startingContext))
			return false;
		if (stack == null) {
			if (other.stack != null)
				return false;
		} else if (!stack.equals(other.stack))
			return false;
		if (standardVersion != other.standardVersion)
			return false;
		return true;
	}
	
}
