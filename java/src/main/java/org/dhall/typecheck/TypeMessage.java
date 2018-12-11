package org.dhall.typecheck;

import java.util.function.Function;

public abstract class TypeMessage<S,A> {

    public <T> T matcher(Matcher<S,A,T> m) {
        return m.apply(this);
    }

    public static class Matcher<S,A,T> implements Function<TypeMessage<S,A>,T>{
        @Override
        public T apply(TypeMessage<S, A> t) {
            return null;
        }
    }

}
