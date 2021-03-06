package org.dhall.core.expr;

import org.dhall.core.Expr;

public final class ExprNaturalBuild<S,A> extends Expr<S, A> {
   
	@Override
    public boolean equals(Object other) {
        return other instanceof ExprNaturalBuild;
    }

    @Override
    public int hashCode() {
        return ExprNaturalBuild.class.hashCode();
    }
}
