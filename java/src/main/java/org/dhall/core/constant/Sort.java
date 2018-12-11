package org.dhall.core.constant;

import org.dhall.core.Const;

public final class Sort extends Const {
    @Override
    public boolean equals(Object other) {
        return other instanceof Sort;
    }

    @Override
    public int hashCode() {
        return Sort.class.hashCode();
    }
}
