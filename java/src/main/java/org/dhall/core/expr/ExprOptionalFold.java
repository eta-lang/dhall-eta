package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprOptionalFold<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprOptionalFold;
    }

    @Override
    public int hashCode() {
        return ExprOptionalFold.class.hashCode();
    }
}
