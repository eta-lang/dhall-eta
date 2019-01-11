package org.dhall.eta;

import java.util.Collection;
import java.util.Map;
import java.util.HashMap;

import org.dhall.common.types.Pair;

/**
 * @author Javier Neira Sanchez
 *
 */
public class FieldTypes {

    @SuppressWarnings("unchecked")
    public static Map<String,Type<Object>>
        create(Collection<Pair<String,Type<? extends Object>>> fieldTypes) {
        Map<String,Type<Object>> objMap = new HashMap<>();
        for (Pair<String,Type<?>> p:fieldTypes)
            objMap.put(p.getFst(), (Type<Object>)p.getSnd());
        return objMap;
    }
    
    @SuppressWarnings("unchecked")
    public static Map<String,Type<Object>>
        upcast(Map<String,Type<? extends Object>> map) {
        Map<String,Type<Object>> objMap = new HashMap<>();
        for (Map.Entry<String,Type<?>> e:map.entrySet())
            objMap.put(e.getKey(), (Type<Object>) e.getValue());
        return objMap;
    }
    
}
