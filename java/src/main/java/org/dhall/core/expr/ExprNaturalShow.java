package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalShow<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalShow;
    }

    @Override
    public int hashCode() {
        return ExprNaturalShow.class.hashCode();
    }
}
