package org.dhall.typecheck;

import java.util.function.Function;

import org.dhall.core.Expr;

public interface Typer<A> extends Function<A,Expr<?,A>> {

}
