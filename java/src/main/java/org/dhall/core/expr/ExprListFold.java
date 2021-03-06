package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListFold<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListFold;
    }

    @Override
    public int hashCode() {
        return ExprListFold.class.hashCode();
    }
}
