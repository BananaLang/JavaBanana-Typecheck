package io.github.bananalang.typecheck;

import javassist.ClassPool;
import javassist.CtClass;
import javassist.NotFoundException;

public final class EvaluatedType {
    public static final EvaluatedType NULL = new EvaluatedType(null, "null", true);

    private final ClassPool cp;
    private final String name;
    private final boolean nullable;
    private CtClass javassistClass;

    public EvaluatedType(ClassPool cp, String name, boolean nullable) {
        this.cp = cp;
        this.name = name;
        this.nullable = nullable;
        javassistClass = null;
    }

    public EvaluatedType(CtClass clazz, boolean nullable) {
        cp = clazz.getClassPool();
        name = clazz.getName();
        this.nullable = nullable;
        javassistClass = clazz;
    }

    public String getName() {
        return name;
    }

    public String getSimpleName() {
        return name.substring(name.lastIndexOf('.'));
    }

    public CtClass getJavassist() {
        if (javassistClass == null) {
            return javassistClass = evaluate();
        }
        return javassistClass;
    }

    public boolean isNullable() {
        return nullable;
    }

    public EvaluatedType nullable(boolean nullable) {
        if (nullable == isNullable()) {
            return this;
        }
        return new EvaluatedType(getJavassist(), nullable);
    }

    private CtClass evaluate() {
        if (cp == null) {
            return null;
        }
        try {
            return cp.get(name);
        } catch (NotFoundException e) {
            throw new IllegalArgumentException("Cannot find class " + name);
        }
    }

    @Override
    public String toString() {
        if (nullable) {
            return name.concat("?");
        }
        return name;
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof EvaluatedType)) {
            return false;
        }
        EvaluatedType other = (EvaluatedType)o;
        return nullable == other.nullable && name.equals(other.name);
    }

    public boolean isAssignableTo(EvaluatedType type) {
        if (nullable && !type.nullable) {
            return false;
        }
        if (equals(type)) {
            return true;
        }
        if (!(type instanceof EvaluatedType)) {
            return false;
        }
        EvaluatedType ljrt = (EvaluatedType)type;
        return Typechecker.checkTypeAssignable(ljrt.getName(), getJavassist());
    }
}
