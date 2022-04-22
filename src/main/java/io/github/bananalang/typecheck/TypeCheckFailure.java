package io.github.bananalang.typecheck;

public class TypeCheckFailure extends IllegalArgumentException {
    public TypeCheckFailure(String reason) {
        super(reason);
    }

    public TypeCheckFailure(Throwable cause) {
        super(cause);
    }
}
