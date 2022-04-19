package io.github.bananalang.typecheck;

public abstract class EvaluatedType {
    public abstract boolean isAssignableTo(EvaluatedType type);
}
