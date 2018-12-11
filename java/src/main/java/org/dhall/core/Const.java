package org.dhall.core;

import java.util.function.Function;

import org.dhall.core.constant.Kind;
import org.dhall.core.constant.Sort;
import org.dhall.core.constant.Type;

public abstract class Const {

    public <T> T match(Matcher<T> m) {
        return m.match(this);
    }
    
    @Override
    public String toString() {
    	return getClass().getSimpleName();
    }
    
	public static class Matcher<T> implements Function<Const, T> {

		private Function<Kind, T> kind;
		private Function<Type, T> type;
		private Function<Sort, T> sort;
		private Function<? super Const, T> any;

		public Matcher(Function<? super Const, T> any) {
			this.any = any;
		}

		public Matcher<T> Kind(Function<Kind, T> f) {
			this.kind = f;
			return this;
		}

		public Matcher<T> Type(Function<Type, T> f) {
			this.type = f;
			return this;
		}

		public Matcher<T> Sort(Function<Sort, T> f) {
			this.sort = f;
			return this;
		}
		
		public T match(Const constant) {
			if (type != null && constant instanceof Type)
				return type.apply((Type) constant);
			else if (kind != null && constant instanceof Kind)
				return kind.apply((Kind) constant);
			else if (sort != null && constant instanceof Sort)
				return sort.apply((Sort) constant);
			else
				return any.apply(constant);
		}

		@Override
		public T apply(Const t) {
			return match(t);
		}
	}

}
