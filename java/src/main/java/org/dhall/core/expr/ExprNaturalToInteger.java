package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalToInteger<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalToInteger;
    }

    @Override
    public int hashCode() {
        return ExprNaturalToInteger.class.hashCode();
    }
}
