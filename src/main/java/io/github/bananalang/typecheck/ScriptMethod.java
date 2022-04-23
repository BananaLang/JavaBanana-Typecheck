package io.github.bananalang.typecheck;

public final class ScriptMethod {
    private final String name;
    private final EvaluatedType returnType;
    private final EvaluatedType[] argTypes;

    public ScriptMethod(String name, EvaluatedType returnType, EvaluatedType[] argTypes) {
        this.name = name;
        this.returnType = returnType;
        this.argTypes = argTypes;
    }

    public String getName() {
        return name;
    }

    public EvaluatedType getReturnType() {
        return returnType;
    }

    public EvaluatedType[] getArgTypes() {
        return argTypes;
    }
}
