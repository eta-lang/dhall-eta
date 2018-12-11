package org.haskell.types;

import java.util.function.BiFunction;
import java.util.function.Function;

public class Pair<F, S> {
	
	public static <F,S> Pair<F,S> of(F fst,S snd) {
		return new Pair<F,S>(fst,snd);
	}
	
	private final F fst;
	private final S snd;
	
	public Pair(F fst, S snd) {
		super();
		this.fst = fst;
		this.snd = snd;
	}

	public F getFst() {
		return fst;
	}

	public S getSnd() {
		return snd;
	}

	public <T> T match(Function<Pair<F,S>,T> f) {
		return f.apply(this);
	}
	
	public <T> T match(BiFunction<F,S,T> f) {
		return f.apply(this.getFst(),this.getSnd());
	}
	
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((fst == null) ? 0 : fst.hashCode());
		result = prime * result + ((snd == null) ? 0 : snd.hashCode());
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
		@SuppressWarnings("unchecked")
		Pair<F,S> other = (Pair<F,S>) obj;
		if (fst == null) {
			if (other.fst != null)
				return false;
		} else if (!fst.equals(other.fst))
			return false;
		if (snd == null) {
			if (other.snd != null)
				return false;
		} else if (!snd.equals(other.snd))
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "Pair [fst="+fst.toString()+", snd="+snd.toString()+"]";
	}
}
