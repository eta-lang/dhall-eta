package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprInteger<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprInteger;
    }

    @Override
    public int hashCode() {
        return ExprInteger.class.hashCode();
    }
}
