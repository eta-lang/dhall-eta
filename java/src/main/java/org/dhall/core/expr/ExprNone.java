package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNone<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNone;
    }

    @Override
    public int hashCode() {
        return ExprNone.class.hashCode();
    }
}
