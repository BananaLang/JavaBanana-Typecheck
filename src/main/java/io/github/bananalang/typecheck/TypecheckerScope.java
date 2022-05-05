package io.github.bananalang.typecheck;

import java.util.HashMap;
import java.util.Map;

import io.github.bananalang.parse.ast.StatementList;

public final class TypecheckerScope {
    private final StatementList owner;
    private final Map<String, LocalVariable> vars;
    private final boolean root;

    public TypecheckerScope(StatementList owner, boolean root) {
        this.owner = owner;
        this.vars = new HashMap<>();
        this.root = root;
    }

    public TypecheckerScope(StatementList owner) {
        this(owner, false);
    }

    public StatementList getOwner() {
        return owner;
    }

    public Map<String, LocalVariable> getVars() {
        return vars;
    }

    public boolean isRoot() {
        return root;
    }
}
