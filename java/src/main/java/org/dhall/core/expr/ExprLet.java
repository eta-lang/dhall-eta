package org.dhall.core.expr;

import org.dhall.common.types.NonEmptyArrayList;
import org.dhall.core.Binding;
import org.dhall.core.Expr;

public final class ExprLet<S, A> extends Expr<S, A> {
	
	private final NonEmptyArrayList<Binding<S,A>> bindings;

	private final Expr<S,A> body;

	public ExprLet(NonEmptyArrayList<Binding<S,A>> bindings, Expr<S, A> body) {
		super();
		this.bindings = bindings;
		this.body = body;
	}

	public NonEmptyArrayList<Binding<S,A>> getBindings() {
		return bindings;
	}

	public Expr<S, A> getBody() {
		return body;
	}

	@Override
	public String toString() {
		return "ExprLet [bindings=" + bindings + ", body=" + body + "]";
	}
	
}	
