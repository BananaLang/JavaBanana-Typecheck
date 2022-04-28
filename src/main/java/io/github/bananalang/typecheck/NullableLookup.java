package io.github.bananalang.typecheck;

import java.util.Map;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.function.Predicate;

import javassist.CtBehavior;
import javassist.CtClass;
import javassist.CtConstructor;
import javassist.CtField;
import javassist.CtMember;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.bytecode.Descriptor;

public final class NullableLookup {
    private static final Map<CtMember, Boolean> NULLABLE_MEMBER_CACHE = new IdentityHashMap<>();
    private static final Map<CtBehavior, boolean[]> NULLABLE_PARAMS_CACHE = new IdentityHashMap<>();

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
            boolean[] result = new boolean[Descriptor.numOfParameters(key.getSignature())];
            Object[][] annotations = key.getAvailableParameterAnnotations();
            for (int i = 0; i < result.length; i++) {
                final int i2 = i;
                Boolean isParamNullable = isNullable0(ann -> {
                    for (Object check : annotations[i2]) {
                        return check.getClass().getName().equals(ann);
                    }
                    return false;
                });
                result[i] = isParamNullable != null ? isParamNullable : true;
            }
            return result;
        });
        return Arrays.copyOf(cachedResult, cachedResult.length);
    }
}
