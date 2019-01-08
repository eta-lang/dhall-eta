/**
 * 
 */
package org.dhall.eta;

import org.dhall.common.types.Pair;

/**
 * @author Javier Neira Sanchez
 *
 */
public class TagValue<A> extends Pair<String,A>{
	
	public TagValue(String fst, A snd) {
		super(fst, snd);
	}
	public String getTag() {
		return getFst();
	}
	public A getValue() {
		return getSnd();
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = super.hashCode();
		result = prime * result + this.getClass().hashCode();
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (!super.equals(obj))
			return false;
		if (getClass() != obj.getClass())
			return false;
		return true;
	}
	
	@Override
	public String toString() {
		return "TagValue [tag="+getTag()+", value="+getValue()+"]";
	}
}
