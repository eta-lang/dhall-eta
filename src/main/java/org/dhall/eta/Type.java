package org.dhall.eta;

import java.util.Optional;

import org.dhall.core.Expr;
import org.dhall.parser.Src;

public interface Type<A> {
	
	public Optional<A> extract(Expr<Src,Void> expr);
	
	public Expr<Src,Void> expected();
}
