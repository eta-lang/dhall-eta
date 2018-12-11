package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprText<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprText;
    }

    @Override
    public int hashCode() {
        return ExprText.class.hashCode();
    }
}
