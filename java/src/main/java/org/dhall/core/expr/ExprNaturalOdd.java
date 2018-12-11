package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalOdd<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalOdd;
    }

    @Override
    public int hashCode() {
        return ExprNaturalOdd.class.hashCode();
    }
}
