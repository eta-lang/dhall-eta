/**
 * 
 */
package org.haskell.types;

/**
 * @author Javier Neira Sanchez
 *
 */
public final class Unit {
	
	public final static Unit instance = new Unit();
	
	private Unit() {
		super();
	}
	
	@Override
	public String toString() {
		return "Unit";
	}
	
}
