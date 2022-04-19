package io.github.bananalang.typecheck;

/**
 * Simple wrapper around {@code EvaluatedType[]} that also extends {@link EvaluatedType}
 */
public final class MultiEvaluatedType extends EvaluatedType {
    private final EvaluatedType[] types;

    public MultiEvaluatedType(EvaluatedType[] types) {
        this.types = types;
    }

    public EvaluatedType[] getTypes() {
        return types;
    }

    @Override
    public boolean isAssignableTo(EvaluatedType type) {
        if (!(type instanceof MultiEvaluatedType)) {
            return false;
        }
        MultiEvaluatedType mType = (MultiEvaluatedType)type;
        if (types.length != mType.types.length) {
            return false;
        }
        for (int i = 0; i < types.length; i++) {
            if (!types[i].isAssignableTo(mType.types[i])) {
                return false;
            }
        }
        return true;
    }
}
