package org.dhall.core.constant;

import org.dhall.core.Const;

public final class Kind extends Const {
    @Override
    public boolean equals(Object other) {
        return other instanceof Kind;
    }

    @Override
    public int hashCode() {
        return Kind.class.hashCode();
    }
}
