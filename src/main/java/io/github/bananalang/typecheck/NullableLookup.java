package io.github.bananalang.typecheck;

import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.Map;
import java.util.function.Predicate;

import javassist.CtBehavior;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMember;
import javassist.CtMethod;
import javassist.NotFoundException;

public final class NullableLookup {
    private static final Map<CtMember, Boolean> NULLABLE_MEMBER_CACHE = new IdentityHashMap<>();
    private static final Map<CtBehavior, boolean[]> NULLABLE_PARAMS_CACHE = new IdentityHashMap<>();

    static {
        try {
            NULLABLE_MEMBER_CACHE.put(Typechecker.CT_JLO.getMethod("getClass", "()Ljava/lang/Class;"), Boolean.FALSE);
        } catch (NotFoundException e) {
            throw new Error(e);
        }
    }

    private NullableLookup() {
    }

    private static Boolean isNullable0(Predicate<String> checkAnnotation) {
        if (checkAnnotation.test("banana.internal.annotation.Nullable")) return true;
        if (checkAnnotation.test("banana.internal.annotation.NonNull")) return false;
        if (checkAnnotation.test("org.jetbrains.annotations.Nullable")) return true;
        if (checkAnnotation.test("org.jetbrains.annotations.NotNull")) return false;
        if (checkAnnotation.test("javax.annotation.Nullable")) return true;
        if (checkAnnotation.test("javax.annotation.NonNull")) return false;
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
            Boolean result = isNullable0(key::hasAnnotation);
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
                    result = isNullable0(altMember::hasAnnotation);
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
            boolean[] result = new boolean[paramTypes.length];
            Object[][] annotations = key.getAvailableParameterAnnotations();
            int foundCount = 0;
            for (int i = 0; i < result.length; i++) {
                if (paramTypes[i].isPrimitive()) {
                    result[i] = false;
                    foundCount++;
                    continue;
                }
                String[] annotationNames = new String[annotations[i].length];
                for (int j = 0; j < annotationNames.length; j++) {
                    annotationNames[j] = annotations[i][j].getClass().getInterfaces()[0].getName();
                }
                Boolean isParamNullable = isNullable0(ann -> {
                    for (String check : annotationNames) {
                        return check.equals(ann);
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
}
