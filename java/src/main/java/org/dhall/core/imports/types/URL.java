package org.dhall.core.imports.types;

import java.util.Optional;

import org.dhall.core.imports.File;
import org.dhall.core.imports.ImportHashed;
import org.dhall.core.imports.types.url.Scheme;

public final class URL {
	
	private final Scheme scheme;
	private final String authority;
	private final File path;
	private final Optional<String> query;
	private final Optional<String> fragment;
	private final Optional<ImportHashed> headers;
	public URL(Scheme scheme, String authority, File path, Optional<String> query, Optional<String> fragment,
			Optional<ImportHashed> headers) {
		super();
		this.scheme = scheme;
		this.authority = authority;
		this.path = path;
		this.query = query;
		this.fragment = fragment;
		this.headers = headers;
	}
	public Scheme getScheme() {
		return scheme;
	}
	public String getAuthority() {
		return authority;
	}
	public File getPath() {
		return path;
	}
	public Optional<String> getQuery() {
		return query;
	}
	public Optional<String> getFragment() {
		return fragment;
	}
	public Optional<ImportHashed> getHeaders() {
		return headers;
	}
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + ((authority == null) ? 0 : authority.hashCode());
		result = prime * result + ((fragment == null) ? 0 : fragment.hashCode());
		result = prime * result + ((headers == null) ? 0 : headers.hashCode());
		result = prime * result + ((path == null) ? 0 : path.hashCode());
		result = prime * result + ((query == null) ? 0 : query.hashCode());
		result = prime * result + ((scheme == null) ? 0 : scheme.hashCode());
		return result;
	}
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		URL other = (URL) obj;
		if (authority == null) {
			if (other.authority != null)
				return false;
		} else if (!authority.equals(other.authority))
			return false;
		if (fragment == null) {
			if (other.fragment != null)
				return false;
		} else if (!fragment.equals(other.fragment))
			return false;
		if (headers == null) {
			if (other.headers != null)
				return false;
		} else if (!headers.equals(other.headers))
			return false;
		if (path == null) {
			if (other.path != null)
				return false;
		} else if (!path.equals(other.path))
			return false;
		if (query == null) {
			if (other.query != null)
				return false;
		} else if (!query.equals(other.query))
			return false;
		if (scheme != other.scheme)
			return false;
		return true;
	}
	
	

	
	
	
}
