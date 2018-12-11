package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListIndexed<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListIndexed;
    }

    @Override
    public int hashCode() {
        return ExprListIndexed.class.hashCode();
    }
}
