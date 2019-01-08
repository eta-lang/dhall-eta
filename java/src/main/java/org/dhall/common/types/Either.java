package org.dhall.common.types;

import java.util.function.Function;

import org.dhall.common.types.either.Left;
import org.dhall.common.types.either.Right;

public abstract class Either<L, R> {
	
	public static <L,R> Either<L,R> left(L val) {
		return new Left<L,R>(val);
	}
	
	public static <L,R> Either<L,R> right(R val) {
		return new Right<L,R>(val);
	}
	
    public <T> T match(Matcher<L,R,T> m) {
    	return m.match(this);
    }
    
	public static class Matcher<L,R,T> implements Function<Either<L,R>, T> {
		private Function<Left<L,R>,T> left;
		private Function<Right<L,R>,T> right;
		private final Function<? super Either<L,R>,T> _any;
		public Matcher(Function<? super Either<L, R>, T> _any) {
			super();
			this._any = _any;
		}
		
		public Matcher<L,R,T> Left(Function<Left<L,R>, T> f) {
			this.left = f;
			return this;
		}

		public Matcher<L,R,T> Right(Function<Right<L,R>, T> f) {
			this.right = f;
			return this;
		}
		
		public T match(Either<L,R> e) {
			if (e instanceof Left && left != null)
				return left.apply((Left<L,R>) e);
			else if (e instanceof Right && right != null)
				return right.apply((Right<L,R>) e);
			else 
				return _any.apply(e);
		}

		@Override
		public T apply(Either<L, R> t) {
			return match(t);
		}
	}
}
