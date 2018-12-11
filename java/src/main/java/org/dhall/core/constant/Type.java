package org.dhall.core.constant;

import org.dhall.core.Const;

public final class Type extends Const {
    @Override
    public boolean equals(Object other) {
        return other instanceof Type;
    }

    @Override
    public int hashCode() {
        return Type.class.hashCode();
    }
}
