package org.dhall;

import java.util.Collections;
import java.util.List;
import java.util.Optional;

import org.haskell.types.Pair;

public class Context<A> {
	
    public static <A> Context<A> empty() {
        return new Context<A>(
           Collections.<Pair<String,A>>emptyList());
    }
	
    private final List<Pair<String,A>> bindings;
    
    public Context(List<Pair<String, A>> bindings) {
        super();
        if (bindings== null)
            throw new IllegalArgumentException("Bindings can't be null!");
        this.bindings = bindings;
    }
    
    public List<Pair<String, A>> getBindings() {
        return bindings;
    }
    
    public void insert(String key, A val) {
        bindings.add(new Pair<String, A>(key, val));
    }
    
    public Optional<Pair<Pair<String,A>,Context<A>>> match() {
        if (bindings.isEmpty())
            return Optional.empty();
        Pair<String,A> last = bindings.get(bindings.size()-1);
        Context<A> restCtx = new Context<A> (bindings.subList(0,bindings.size()-2));
        return Optional.of(new Pair<Pair<String,A>, Context<A>>(last,restCtx));
    }
    
    public Optional<A> lookup(String key, Integer idx) {
        if (idx < bindings.size()) {
            Pair<String,A> e = bindings.get(idx);
            if (e.getFst().equals(key))
                return Optional.of(e.getSnd());
        }
        return Optional.empty();
    }
    
    public List<Pair<String, A>> toList() {
        return getBindings();
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((bindings == null) ? 0 : bindings.hashCode());
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
            Context<A> other = (Context<A>) obj;
        if (bindings == null) {
            if (other.bindings != null)
                return false;
        } else if (!bindings.equals(other.bindings))
            return false;
        return true;
    }
    
}
