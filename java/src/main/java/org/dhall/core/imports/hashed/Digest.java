package org.dhall.core.imports.hashed;

import java.util.Arrays;

public final class Digest<A> {
	
    private final byte[] hash; 
    
    public Digest(byte[] hash) {
        super();
        this.hash = hash;
    }

    public byte[] getHash() {
        return hash;
    }
    
    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + Arrays.hashCode(hash);
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
		Digest<A> other = (Digest<A>) obj;
        if (!Arrays.equals(hash, other.hash))
            return false;
        return true;
    }

	@Override
	public String toString() {
		return "Digest [hash=" + Arrays.toString(hash) + "]";
	}
    
    
}
