package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListLength<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListLength;
    }

    @Override
    public int hashCode() {
        return ExprListLength.class.hashCode();
    }
}
