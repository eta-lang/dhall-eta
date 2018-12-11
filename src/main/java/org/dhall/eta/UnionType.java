package org.dhall.eta;

import java.util.Map;

/**
 * @author Javier Neira Sanchez
 *
 */
public interface UnionType<A> {
	
	Map<String,Type<? extends Object>> getTagsTypes();
	
	A fromTagValue(TagValue<Object> tagVal);
}
