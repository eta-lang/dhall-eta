package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalFold<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalFold;
    }

    @Override
    public int hashCode() {
        return ExprNaturalFold.class.hashCode();
    }
}
