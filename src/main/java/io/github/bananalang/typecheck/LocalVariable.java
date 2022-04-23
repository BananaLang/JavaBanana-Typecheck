package io.github.bananalang.typecheck;

public final class LocalVariable {
    private final String name;
    private final EvaluatedType type;
    private final int index;
    private boolean assigned;

    public LocalVariable(String name, EvaluatedType type, int index, boolean assigned) {
        this.name = name;
        this.type = type;
        this.index = index;
        this.assigned = assigned;
    }

    public String getName() {
        return name;
    }

    public EvaluatedType getType() {
        return type;
    }

    public int getIndex() {
        return index;
    }

    public boolean isAssigned() {
        return assigned;
    }

    public void setAssigned(boolean assigned) {
        this.assigned = assigned;
    }
}
