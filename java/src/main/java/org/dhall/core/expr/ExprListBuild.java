package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprListBuild<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprListBuild;
    }

    @Override
    public int hashCode() {
        return ExprListBuild.class.hashCode();
    }
}
