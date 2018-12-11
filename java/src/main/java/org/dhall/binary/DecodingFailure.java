/**
 * 
 */
package org.dhall.binary;

import java.util.function.Function;

/**
 * @author Javier Neira Sanchez
 *
 */
public abstract class DecodingFailure {
	
	
	public <T> T matcher(Matcher<T> m) {
        return m.apply(this);
    }

    public static class Matcher<T> implements Function<DecodingFailure,T>{
        @Override
        public T apply(DecodingFailure f) {
            return null;
        }
    }
}
