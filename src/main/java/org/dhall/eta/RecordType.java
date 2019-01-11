package org.dhall.eta;

import java.util.Map;

/**
 * @author Javier Neira Sanchez
 *
 */
public interface RecordType<A> {
	
	Map<String,Type<? extends Object>> getFieldsTypes();
	
	A fromFieldsValues(Map<String,Object> fields);
	
}
