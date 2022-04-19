package io.github.bananalang.typecheck;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.NotFoundException;

public class Imported<T> {
    public static enum ImportType {
        CLASS,
        STATIC_FIELD,
        STATIC_METHOD
    }

    private final CtClass ownedClass;
    private final T object;
    private final String shortName;
    private final ImportType type;

    private Imported(CtClass ownedClass, T object, String shortName, ImportType type) {
        this.ownedClass = ownedClass;
        this.object = object;
        this.shortName = shortName;
        this.type = type;
    }

    public static Imported<CtClass> class_(CtClass clazz) {
        return new Imported<>(clazz, clazz, clazz.getSimpleName(), ImportType.CLASS);
    }

    public static Imported<CtClass> class_(ClassPool cp, String name) {
        try {
            return class_(cp.get(name));
        } catch (NotFoundException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public static Collection<Imported<?>> starImport(CtClass clazz) {
        List<Imported<?>> imports = new ArrayList<>();
        for (CtField field : clazz.getDeclaredFields()) {
            if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())) {
                imports.add(new Imported<>(clazz, field, field.getName(), ImportType.STATIC_FIELD));
            }
        }
        Set<String> foundFields = new HashSet<>();
        for (CtMethod method : clazz.getDeclaredMethods()) {
            if (Modifier.isPublic(method.getModifiers()) && Modifier.isStatic(method.getModifiers())) {
                if (foundFields.add(method.getName())) {
                    imports.add(new Imported<>(clazz, method, method.getName(), ImportType.STATIC_METHOD));
                }
            }
        }
        return imports;
    }

    public static Collection<Imported<?>> starImport(ClassPool cp, String name) {
        try {
            return starImport(cp.get(name));
        } catch (NotFoundException e) {
            throw new IllegalArgumentException(e);
        }
    }

    public CtClass getOwnedClass() {
        return ownedClass;
    }

    public T getObject() {
        return object;
    }

    public String getShortName() {
        return shortName;
    }

    public ImportType getType() {
        return type;
    }
}
