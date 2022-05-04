package io.github.bananalang.typecheck;

import java.util.Set;

import io.github.bananalang.parse.ast.VariableDeclarationStatement.Modifier;

public final class GlobalVariable {
    private final String name;
    private EvaluatedType type;
    private final Set<Modifier> modifiers;

    public GlobalVariable(String name, EvaluatedType type, Set<Modifier> modifiers) {
        this.name = name;
        this.type = type;
        this.modifiers = modifiers;
    }

    public String getName() {
        return name;
    }

    public EvaluatedType getType() {
        return type;
    }

    void setType(EvaluatedType type) {
        this.type = type;
    }

    public Set<Modifier> getModifiers() {
        return modifiers;
    }
}
