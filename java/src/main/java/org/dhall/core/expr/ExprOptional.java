package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprOptional<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprOptional;
    }

    @Override
    public int hashCode() {
        return ExprOptional.class.hashCode();
    }
}
