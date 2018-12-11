package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalEven<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalEven;
    }

    @Override
    public int hashCode() {
        return ExprNaturalEven.class.hashCode();
    }
}
