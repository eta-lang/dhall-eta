package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprList<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprList;
    }

    @Override
    public int hashCode() {
        return ExprList.class.hashCode();
    }
}
