/**
 * 
 */
package org.dhall.typecheck;

import org.dhall.Context;
import org.dhall.core.Expr;

public class DetailedTypeError<S,A> extends TypeError<S,A> {

	public DetailedTypeError(Context<A> context, Expr<S,A> current, TypeMessage<S,A> message) {
		super(context, current, message);
	}

}
