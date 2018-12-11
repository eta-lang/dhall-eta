package org.dhall.core.expr;

import java.util.Optional;

import org.dhall.core.Expr;

public interface OptionallyTyped<S,A> {
	Optional<Expr<S,A>> getType();
}
