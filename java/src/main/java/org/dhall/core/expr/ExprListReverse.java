package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListReverse<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListReverse;
    }

    @Override
    public int hashCode() {
        return ExprListReverse.class.hashCode();
    }
}
