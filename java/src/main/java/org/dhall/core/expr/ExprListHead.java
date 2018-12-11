package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListHead<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListHead;
    }

    @Override
    public int hashCode() {
        return ExprListHead.class.hashCode();
    }
}
