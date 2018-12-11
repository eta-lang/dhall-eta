package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprIntegerShow<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprIntegerShow;
    }

    @Override
    public int hashCode() {
        return ExprIntegerShow.class.hashCode();
    }
}
