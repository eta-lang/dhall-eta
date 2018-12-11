package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListLast<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListLast;
    }

    @Override
    public int hashCode() {
        return ExprListLast.class.hashCode();
    }
}
