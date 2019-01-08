package org.dhall.core.imports;

import java.util.function.Function;

import org.dhall.core.imports.types.Env;
import org.dhall.core.imports.types.Local;
import org.dhall.core.imports.types.Missing;
import org.dhall.core.imports.types.Remote;

public abstract class ImportType {
	
	public <R> R matcher(Matcher<R> m) {
        return m.apply(this);
    }
	
	public static class Matcher<R> implements Function<ImportType, R> {
		
		private Function<Env, R> env;
		private Function<Local, R> local;
		private Function<Missing, R> missing;
		private Function<Remote, R> remote;
		private final Function<ImportType, R> _any;
		
		public Matcher(Function<ImportType, R> _any) {
			super();
			this._any = _any;
		}

		public Matcher<R> Env(Function<Env, R> env) {
			this.env = env;
			return this;
		}
		
		public Matcher<R> Local(Function<Local, R> local) {
			this.local = local;
			return this;
		}
		
		public Matcher<R> Missing(Function<Missing, R> missing) {
			this.missing = missing;
			return this;
		}
		
		public Matcher<R> Remote(Function<Remote, R> remote) {
			this.remote = remote;
			return this;
		}
		
		public R match(ImportType instance) {
			if (env != null && instance instanceof Env) {
				return env.apply((Env) instance);
			} else if (local != null && instance instanceof Local) {
				return local.apply((Local) instance);
			} else if (missing != null && instance instanceof Missing) {
				return missing.apply((Missing) instance);
			} else if (remote != null && instance instanceof Remote) {
				return remote.apply((Remote) instance);
			} else {
				return _any.apply(instance);
			}
		}

		@Override
		public R apply(ImportType instance) {
			return match(instance);
		}
		
	}
}
