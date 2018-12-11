package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalIsZero<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalIsZero;
    }

    @Override
    public int hashCode() {
        return ExprNaturalIsZero.class.hashCode();
    }
}
