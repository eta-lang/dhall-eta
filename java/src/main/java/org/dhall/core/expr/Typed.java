package org.dhall.core.expr;

import org.dhall.core.Expr;

public interface Typed<S,A> {
	Expr<S,A> getType();
}
