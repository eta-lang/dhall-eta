package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class MustMergeUnion<S,A> extends AnnotatedExpressionMessage<S,A> {
	
	public MustMergeUnion(Expr<S, A> expression, Expr<S, A> annot) {
		super(expression, annot);
	}

}
