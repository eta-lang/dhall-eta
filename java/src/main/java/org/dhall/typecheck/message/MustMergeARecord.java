package org.dhall.typecheck.message;

import org.dhall.core.Expr;

public final class MustMergeARecord<S,A> extends AnnotatedExpressionMessage<S,A> {
	
	public MustMergeARecord(Expr<S, A> expression, Expr<S, A> annot) {
		super(expression,annot);
	}

}
