package io.github.bananalang.typecheck;

import io.github.bananalang.parse.ast.VariableDeclarationStatement.Modifier;

/**
 * Redirection to be able to access Banana's Modifier class in the same file as Javassist's
 * @see Modifier
 * @see javassist.Modifier
 */
public final class Modifier2 {
    private Modifier2() {
    }

    public static final Modifier PUBLIC = Modifier.PUBLIC;
    public static final Modifier GLOBAL = Modifier.GLOBAL;
    public static final Modifier LAZY = Modifier.LAZY;
    public static final Modifier EXTENSION = Modifier.EXTENSION;

    public static Modifier fromName(String name) {
        return Modifier.fromName(name);
    }
}
