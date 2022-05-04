package io.github.bananalang.typecheck;

import java.util.Set;

import io.github.bananalang.parse.ast.VariableDeclarationStatement.Modifier;

public final class ScriptMethod {
    private final String name;
    private EvaluatedType returnType;
    private final EvaluatedType[] argTypes;
    private final Set<Modifier> modifiers;

    public ScriptMethod(String name, EvaluatedType returnType, EvaluatedType[] argTypes, Set<Modifier> modifiers) {
        this.name = name;
        this.returnType = returnType;
        this.argTypes = argTypes;
        this.modifiers = modifiers;
    }

    public String getName() {
        return name;
    }

    public EvaluatedType getReturnType() {
        return returnType;
    }

    void setReturnType(EvaluatedType returnType) {
        this.returnType = returnType;
    }

    public EvaluatedType[] getArgTypes() {
        return argTypes;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }
}
