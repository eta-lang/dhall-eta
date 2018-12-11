package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprBool<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprBool;
    }

    @Override
    public int hashCode() {
        return ExprBool.class.hashCode();
    }
    
}
