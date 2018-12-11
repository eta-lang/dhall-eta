
package org.dhall.core.imports.types;

import org.dhall.core.imports.ImportType;

/**
 * @author Javier Neira Sanchez
 *
 */
public class Missing extends ImportType {
	
	@Override
	public boolean equals(Object other) {
		return other instanceof Missing;
	}

	@Override
	public int hashCode() {
		return Missing.class.hashCode();
	}
}
