package io.github.bananalang.typecheck;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import banana.internal.annotation.BananaModule;
import io.github.bananalang.util.BananaUtils;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.Modifier;
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
            throw new TypeCheckFailure(e);
        }
    }

    public static Collection<Imported<?>> starImport(CtClass clazz) {
        List<Imported<?>> imports = new ArrayList<>();
        try {
            BananaModule moduleDesc = (BananaModule)clazz.getAnnotation(BananaModule.class);
            if (moduleDesc != null) {
                for (Class<?> exportedClass : moduleDesc.exportedClasses()) {
                    imports.add(class_(clazz.getClassPool(), exportedClass.getName()));
                }
                for (String publicImport : moduleDesc.publicImports()) {
                    int dotIndex = publicImport.indexOf('.');
                    if (dotIndex == -1) {
                        throw new TypeCheckFailure(
                            "Error importing module " + clazz.getName() +
                            ": Public import " + publicImport + " missing dot"
                        );
                    }
                    imports.addAll(infer(
                        clazz.getClassPool(),
                        publicImport.substring(0, dotIndex),
                        publicImport.substring(dotIndex + 1)
                    ));
                }
            }
        } catch (ClassNotFoundException e) {
        }
        for (CtField field : clazz.getDeclaredFields()) {
            if (Modifier.isPublic(field.getModifiers()) && Modifier.isStatic(field.getModifiers())) {
                imports.add(new Imported<>(clazz, field, field.getName(), ImportType.STATIC_FIELD));
            }
        }
        Set<String> foundMethods = new HashSet<>();
        for (CtMethod method : clazz.getDeclaredMethods()) {
            if (Modifier.isPublic(method.getModifiers()) && Modifier.isStatic(method.getModifiers())) {
                if (foundMethods.add(method.getName())) {
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
            throw new TypeCheckFailure(e);
        }
    }

    public static Collection<Imported<?>> infer(ClassPool cp, String module, String name) {
        module = module.replace('/', '.');
        String moduleClassName = BananaUtils.moduleToClassName(module);
        CtClass clazz = cp.getOrNull(moduleClassName);
        // 1. Check if it's an exported member of a module
        if (clazz != null) {
            if (name.equals("*")) {
                return starImport(clazz);
            }
            try {
                BananaModule moduleDesc = (BananaModule)clazz.getAnnotation(BananaModule.class);
                if (moduleDesc != null) {
                    for (Class<?> exportedClass : moduleDesc.exportedClasses()) {
                        if (exportedClass.getSimpleName().equals(name)) {
                            return Collections.singleton(class_(cp, exportedClass.getName()));
                        }
                    }
                }
            } catch (ClassNotFoundException e) {
            }
            try {
                return Collections.singleton(new Imported<>(clazz, clazz.getDeclaredMethod(name), name, ImportType.STATIC_METHOD));
            } catch (NotFoundException e) {
            }
            try {
                return Collections.singleton(new Imported<>(clazz, clazz.getDeclaredField(name), name, ImportType.STATIC_FIELD));
            } catch (NotFoundException e) {
            }
        }
        // 2. Check if it's a class in a package
        if (!name.equals("*")) {
            clazz = cp.getOrNull(module + '.' + name);
            if (clazz != null) {
                return Collections.singleton(class_(clazz));
            }
        }
        // 3. Check if it's a public static member in a class
        clazz = cp.getOrNull(module);
        if (clazz != null) {
            if (name.equals("*")) {
                return starImport(clazz);
            }
            try {
                return Collections.singleton(new Imported<>(clazz, clazz.getDeclaredMethod(name), name, ImportType.STATIC_METHOD));
            } catch (NotFoundException e) {
            }
            try {
                return Collections.singleton(new Imported<>(clazz, clazz.getDeclaredField(name), name, ImportType.STATIC_FIELD));
            } catch (NotFoundException e) {
            }
        }
        throw new TypeCheckFailure("Could not find import " + module + '.' + name);
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
