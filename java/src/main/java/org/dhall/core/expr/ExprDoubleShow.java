package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprDoubleShow<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprDoubleShow;
    }

    @Override
    public int hashCode() {
        return ExprDoubleShow.class.hashCode();
    }
}
