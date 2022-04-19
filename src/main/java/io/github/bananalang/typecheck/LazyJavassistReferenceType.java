package io.github.bananalang.typecheck;

import java.util.ArrayDeque;
import java.util.Deque;

import javassist.ClassPool;
import javassist.CtClass;
import javassist.NotFoundException;

public final class LazyJavassistReferenceType extends EvaluatedType {
    private final ClassPool cp;
    private final String name;
    private CtClass javassistClass;

    public LazyJavassistReferenceType(ClassPool cp, String name) {
        this.cp = cp;
        this.name = name;
        javassistClass = null;
    }

    public String getName() {
        return name;
    }

    public CtClass getJavassist() {
        if (javassistClass == null) {
            evaluate();
        }
        return javassistClass;
    }

    private void evaluate() {
        try {
            javassistClass = cp.get(name);
        } catch (NotFoundException e) {
            throw new IllegalArgumentException("Cannot find class " + name);
        }
    }

    @Override
    public boolean equals(Object o) {
        if (!(o instanceof LazyJavassistReferenceType)) {
            return false;
        }
        return name.equals(((LazyJavassistReferenceType)o).name);
    }

    @Override
    public boolean isAssignableTo(EvaluatedType type) {
        if (equals(type)) {
            return true;
        }
        if (!(type instanceof LazyJavassistReferenceType)) {
            return false;
        }
        LazyJavassistReferenceType ljrt = (LazyJavassistReferenceType)type;
        String otherName = ljrt.getName();
        Deque<CtClass> toSearch = new ArrayDeque<>();
        toSearch.add(getJavassist());
        while (!toSearch.isEmpty()) {
            CtClass tryClass = toSearch.poll();
            if (tryClass.getName().equals(otherName)) {
                return true;
            }
            if (!tryClass.isInterface()) {
                try {
                    CtClass superclass = tryClass.getSuperclass();
                    if (superclass != null) {
                        toSearch.add(superclass);
                    }
                } catch (NotFoundException e) {
                    throw new IllegalArgumentException(e);
                }
            }
            try {
                for (CtClass intf : tryClass.getInterfaces()) {
                    toSearch.add(intf);
                }
            } catch (NotFoundException e) {
                throw new IllegalArgumentException(e);
            }
        }
        return false;
    }
}
