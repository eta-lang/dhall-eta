package org.dhall.eta;

import org.dhall.core.Expr;
import org.dhall.parser.Src;

/**
 * @author Javier Neira Sanchez
 *
 */
public interface InputType<A> {
	
	public Expr<Src,Void> embed(A a);
	
	public Expr<Src,Void> declared();
}
