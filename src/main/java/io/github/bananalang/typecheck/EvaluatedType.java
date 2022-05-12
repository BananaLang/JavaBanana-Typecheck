package io.github.bananalang.typecheck;

import javassist.ClassPool;
import javassist.CtClass;
import javassist.NotFoundException;
import javassist.bytecode.Descriptor;

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

    private EvaluatedType(ClassPool cp, String name, boolean nullable, CtClass javassistClass) {
        this.cp = cp;
        this.name = name;
        this.nullable = nullable;
        this.javassistClass = javassistClass;
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
        if (this == NULL || nullable == this.nullable || name.equals("void")) {
            return this;
        }
        // Call getJavassist() to evaluate the type now once, rather than potentially doing it twice
        // (once for this and once for the copy) later.
        return new EvaluatedType(cp, name, nullable, getJavassist());
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

    public String getJvmName() {
        return this == NULL ? "java/lang/Void" : Descriptor.toJvmName(getJavassist());
    }

    public String getDescriptor() {
        return this == NULL ? "Ljava/lang/Void;" : Descriptor.of(getJavassist());
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
        if (this == o) {
            return true;
        }
        EvaluatedType other = (EvaluatedType)o;
        return nullable == other.nullable && name.equals(other.name);
    }

    public boolean isAssignableTo(EvaluatedType type) {
        if (this == NULL) {
            return type.nullable;
        }
        if (type == NULL) {
            return false; // Cannot assign anything but literal null to inferred literal null type
        }
        if (nullable && !type.nullable) {
            return false;
        }
        if (name.equals(type.name)) {
            return true;
        }
        EvaluatedType ljrt = (EvaluatedType)type;
        return Typechecker.checkTypeAssignable(ljrt.getName(), getJavassist());
    }
}
