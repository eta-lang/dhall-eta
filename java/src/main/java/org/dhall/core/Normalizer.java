package org.dhall.core;

import java.util.Optional;
import java.util.function.Function;

public interface Normalizer<A> extends Function<Expr<?,A>, Optional<Expr<?,A>>> {

}
