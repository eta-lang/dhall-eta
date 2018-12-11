package org.dhall.core;

import java.util.List;
import java.util.function.Function;

import org.haskell.types.Pair;

public class Chunks<S,A> {
    
    private final List<Pair<String,Expr<S,A>>> interpolations;
    private final String text;
    
    
    public Chunks(List<Pair<String, Expr<S, A>>> inters, String text) {
        super();
        this.interpolations = inters;
        this.text = text;
    }
    
    public List<Pair<String, Expr<S, A>>> getInterpolations() {
        return interpolations;
    }

    public String getText() {
        return text;
    }
    
    public <T> T match(Function<Chunks<S,A>, T> f) {
        return f.apply(this);
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((interpolations == null) ? 0 : interpolations.hashCode());
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
            Chunks<S,A> other = (Chunks<S,A>) obj;
        if (interpolations == null) {
            if (other.interpolations != null)
                return false;
        } else if (!interpolations.equals(other.interpolations))
            return false;
        return true;
    }

	@Override
	public String toString() {
		return "Chunks [interpolations=" + interpolations + ", text=" + text + "]";
	}
    
    
}
