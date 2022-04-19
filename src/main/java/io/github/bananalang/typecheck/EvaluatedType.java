package io.github.bananalang.typecheck;

import javassist.ClassPool;
import javassist.CtClass;
import javassist.NotFoundException;

public final class EvaluatedType {
    private final ClassPool cp;
    private final String name;
    private CtClass javassistClass;

    public EvaluatedType(ClassPool cp, String name) {
        this.cp = cp;
        this.name = name;
        javassistClass = null;
    }

    public EvaluatedType(CtClass clazz) {
        cp = clazz.getClassPool();
        name = clazz.getName();
        javassistClass = clazz;
    }

    public String getName() {
        return name;
    }

    public CtClass getJavassist() {
        if (javassistClass == null) {
            return javassistClass = evaluate();
        }
        return javassistClass;
    }

    private CtClass evaluate() {
        try {
            return cp.get(name);
        } catch (NotFoundException e) {
            throw new IllegalArgumentException("Cannot find class " + name);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof EvaluatedType)) {
            return false;
        }
        return name.equals(((EvaluatedType)o).name);
    }

    public boolean isAssignableTo(EvaluatedType type) {
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
