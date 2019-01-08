/**
 * 
 */
package org.dhall.binary;

import java.util.function.Function;

import org.dhall.binary.decoding.failure.CBORIsNotDhall;
import org.dhall.binary.decoding.failure.CannotDecodeVersionString;
import org.dhall.binary.decoding.failure.UnsupportedVersionString;

/**
 * @author Javier Neira Sanchez
 *
 */
public abstract class DecodingFailure {
	
	
	public <R> R matcher(Matcher<R> m) {
        return m.apply(this);
    }

    public static class Matcher<R> implements Function<DecodingFailure,R>{
        
    	private Function<CannotDecodeVersionString,R> cannotDecodeVersion;
    	private Function<CBORIsNotDhall,R> cborIsNotDhall;
    	private Function<UnsupportedVersionString,R> unsupportedVersion;
    	
    	private final Function<? super DecodingFailure,R> _any;
    	
    	public Matcher(Function<? super DecodingFailure, R> _any) {
			super();
			this._any = _any;
		}

    	public Matcher<R> CannotDecodeVersion(Function<CannotDecodeVersionString,R> f) {
    		this.cannotDecodeVersion = f;
    		return this;
    	}

    	public Matcher<R> CBORIsNotDhall(Function<CBORIsNotDhall,R> cborIsNotDhall) {
    		this.cborIsNotDhall = cborIsNotDhall;
    		return this;
    	}
    	

    	public Matcher<R> UnsupportedVersion(
    			Function<UnsupportedVersionString,R> unsupportedVersion) {
    		this.unsupportedVersion = unsupportedVersion;
    		return this;
    	}
    	
    	public R match(DecodingFailure instance) {
    		if (cannotDecodeVersion !=null && instance instanceof CannotDecodeVersionString) {
    			return cannotDecodeVersion.apply((CannotDecodeVersionString) instance);
    		} else if (cborIsNotDhall !=null && instance instanceof CBORIsNotDhall) {
    			return cborIsNotDhall.apply((org.dhall.binary.decoding.failure.CBORIsNotDhall) instance);
    		} else if (unsupportedVersion !=null && instance instanceof UnsupportedVersionString) {
    			return unsupportedVersion.apply((UnsupportedVersionString) instance);
    		} else {
    			return _any.apply(instance);
    		}
    	}
    	
		@Override
        public R apply(DecodingFailure instance) {
            return match(instance);
        }
    }
}
