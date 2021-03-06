package io.github.bananalang.typecheck;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import javassist.CtBehavior;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMember;
import javassist.CtMethod;
import javassist.Modifier;
import javassist.NotFoundException;

public final class InformationLookup {
    private static final class IdentityValuePair<K, V> {
        final K key;
        final V value;

        IdentityValuePair(K key, V value) {
            this.key = key;
            this.value = value;
        }

        @Override
        public int hashCode() {
            return System.identityHashCode(key) ^ value.hashCode();
        }

        @Override
        public boolean equals(Object o) {
            if (!(o instanceof IdentityValuePair)) return false;
            IdentityValuePair<?, ?> other = (IdentityValuePair<?, ?>)o;
            return key == other.key && value.equals(other.value);
        }
    }

    private static final boolean[] EMPTY_BOOLEAN_ARRAY = new boolean[0];

    private static final Map<IdentityValuePair<CtMember, String>, Boolean> HAS_ANNOTATION_CACHE = new HashMap<>();
    private static final Map<CtMember, Boolean> NULLABLE_MEMBER_CACHE = new IdentityHashMap<>();
    private static final Map<CtBehavior, boolean[]> NULLABLE_PARAMS_CACHE = new IdentityHashMap<>();

    static {
        try {
            NULLABLE_MEMBER_CACHE.put(Typechecker.CT_JLO.getMethod("getClass", "()Ljava/lang/Class;"), Boolean.FALSE);
        } catch (NotFoundException e) {
            throw new Error(e);
        }
    }

    private InformationLookup() {
    }

    public static boolean hasAnnotation(CtMember member, String annotation) {
        return HAS_ANNOTATION_CACHE.computeIfAbsent(
            new IdentityValuePair<>(member, annotation),
            k -> k.key.hasAnnotation(k.value)
        );
    }

    private static Boolean isNullable0(Predicate<String> checkAnnotation) {
        if (checkAnnotation.test("banana.internal.annotation.NonNull")) return false;
        if (checkAnnotation.test("banana.internal.annotation.Nullable")) return true;
        if (checkAnnotation.test("org.jetbrains.annotations.NotNull")) return false;
        if (checkAnnotation.test("org.jetbrains.annotations.Nullable")) return true;
        if (checkAnnotation.test("javax.annotation.NonNull")) return false;
        if (checkAnnotation.test("javax.annotation.Nullable")) return true;
        return null;
    }

    public static boolean isNullable(CtMember member) {
        if (
            member instanceof CtConstructor ||
            (member instanceof CtMethod && ((CtMethod)member).getSignature().endsWith(")V"))
        ) {
            return false; // void isn't nullable
        }
        return NULLABLE_MEMBER_CACHE.computeIfAbsent(member, key -> {
            Boolean result = isNullable0(ann -> hasAnnotation(key, ann));
            if (result == null) {
                String altMemberClassName = "banana.stubs." + key.getDeclaringClass().getName();
                try {
                    CtClass altMemberClass = key.getDeclaringClass().getClassPool().get(altMemberClassName);
                    CtMember altMember;
                    if (key instanceof CtField) {
                        altMember = altMemberClass.getDeclaredField(key.getName(), key.getSignature());
                    } else if (key instanceof CtConstructor) {
                        altMember = altMemberClass.getConstructor(key.getSignature());
                    } else if (key instanceof CtMethod) {
                        altMember = altMemberClass.getMethod(key.getName(), key.getSignature());
                    } else {
                        throw new AssertionError(key.getClass());
                    }
                    result = isNullable0(ann -> hasAnnotation(altMember, ann));
                } catch (NotFoundException e) {
                }
            }
            return result != null ? result : Boolean.TRUE;
        });
    }

    public static boolean[] nullableParams(CtBehavior behavior) {
        boolean[] cachedResult = NULLABLE_PARAMS_CACHE.computeIfAbsent(behavior, key -> {
            CtClass[] paramTypes;
            try {
                paramTypes = key.getParameterTypes();
            } catch (NotFoundException e) {
                throw new TypeCheckFailure(e);
            }
            if (paramTypes.length == 0) {
                return EMPTY_BOOLEAN_ARRAY;
            }
            boolean[] result = new boolean[paramTypes.length];
            int primitiveCount = 0;
            for (CtClass paramType : paramTypes) {
                if (paramType.isPrimitive()) {
                    primitiveCount++;
                }
            }
            if (primitiveCount == paramTypes.length) {
                // ALl primitive arguments!
                return result;
            }
            Object[][] annotations = key.getAvailableParameterAnnotations();
            int foundCount = 0;
            for (int i = 0; i < result.length; i++) {
                if (paramTypes[i].isPrimitive()) {
                    foundCount++;
                    continue;
                }
                String[] annotationNames = new String[annotations[i].length];
                for (int j = 0; j < annotationNames.length; j++) {
                    annotationNames[j] = annotations[i][j].getClass().getInterfaces()[0].getName();
                }
                Boolean isParamNullable = isNullable0(ann -> {
                    for (String check : annotationNames) {
                        if (check.equals(ann)) {
                            return true;
                        }
                    }
                    return false;
                });
                if (isParamNullable != null) {
                    result[i] = isParamNullable;
                    foundCount++;
                } else {
                    result[i] = true;
                }
            }
            if (foundCount == 0) {
                // Do a stub lookup
                String altMemberClassName = "banana.stubs." + key.getDeclaringClass().getName();
                try {
                    CtClass altMemberClass = key.getDeclaringClass().getClassPool().get(altMemberClassName);
                    CtBehavior altMember;
                    if (key instanceof CtConstructor) {
                        altMember = altMemberClass.getConstructor(key.getSignature());
                    } else if (key instanceof CtMethod) {
                        altMember = altMemberClass.getMethod(key.getName(), key.getSignature());
                    } else {
                        throw new AssertionError(key.getClass());
                    }
                    Object[][] altAnnotations = altMember.getAvailableParameterAnnotations();
                    for (int i = 0; i < result.length; i++) {
                        String[] annotationNames = new String[altAnnotations[i].length];
                        for (int j = 0; j < annotationNames.length; j++) {
                            annotationNames[j] = altAnnotations[i][j].getClass().getInterfaces()[0].getName();
                        }
                        Boolean isParamNullable = isNullable0(ann -> {
                            for (String check : annotationNames) {
                                return check.equals(ann);
                            }
                            return false;
                        });
                        if (isParamNullable != null) {
                            result[i] = isParamNullable;
                        }
                    }
                } catch (NotFoundException e) {
                }
            }
            return result;
        });
        return Arrays.copyOf(cachedResult, cachedResult.length);
    }

    public static CtMethod getFunctionalInterfaceMethod(CtClass clazz) {
        if (!clazz.isInterface()) {
            return null;
        }
        CtMethod result = null;
        for (CtMethod method : clazz.getDeclaredMethods()) {
            int modifiers = method.getModifiers();
            if (!Modifier.isAbstract(modifiers)) continue;
            if (result == null) {
                result = method;
            } else {
                return null;
            }
        }
        return result;
    }

    public static CtMethod[] findFunctionalInterfaceMethods(CtClass clazz) {
        List<CtMethod> result = new ArrayList<>();
        Typechecker.forEachSuperclass(clazz, c -> {
            CtMethod method = getFunctionalInterfaceMethod(c);
            if (method != null) {
                result.add(method);
            }
            return true;
        });
        return result.toArray(new CtMethod[result.size()]);
    }
}
