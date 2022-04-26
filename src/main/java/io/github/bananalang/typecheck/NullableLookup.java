package io.github.bananalang.typecheck;

import java.util.Map;
import java.util.Arrays;
import java.util.IdentityHashMap;
import java.util.function.Predicate;

import javassist.CtBehavior;
import javassist.CtMember;
import javassist.bytecode.Descriptor;

public final class NullableLookup {
    private static final Map<CtMember, Boolean> NULLABLE_MEMBER_CACHE = new IdentityHashMap<>();
    private static final Map<CtBehavior, boolean[]> NULLABLE_PARAMS_CACHE = new IdentityHashMap<>();

    private NullableLookup() {
    }

    public static boolean isNullable(Predicate<String> checkAnnotation) {
        for (
            String nullableAnnotation : new String[] {
                "banana.internal.annotation.Nullable",
                "org.jetbrains.annotations.Nullable",
                "javax.annotation.Nullable"
            }
        ) {
            if (checkAnnotation.test(nullableAnnotation)) {
                return true;
            }
        }
        for (
            String nonnullAnnotation : new String[] {
                "banana.internal.annotation.NonNull",
                "org.jetbrains.annotations.NotNull",
                "javax.annotation.NonNull"
            }
        ) {
            if (checkAnnotation.test(nonnullAnnotation)) {
                return false;
            }
        }
        return true; // Default fallback
    }

    public static boolean isNullable(CtMember member) {
        return NULLABLE_MEMBER_CACHE.computeIfAbsent(member, key -> isNullable(key::hasAnnotation));
    }

    public static boolean[] nullableParams(CtBehavior behavior) {
        boolean[] cachedResult = NULLABLE_PARAMS_CACHE.computeIfAbsent(behavior, key -> {
            boolean[] result = new boolean[Descriptor.numOfParameters(behavior.getSignature())];
            Object[][] annotations = behavior.getAvailableParameterAnnotations();
            for (int i = 0; i < result.length; i++) {
                final int i2 = i;
                result[i] = isNullable(ann -> {
                    for (Object check : annotations[i2]) {
                        return check.getClass().getName().equals(ann);
                    }
                    return false;
                });
            }
            return result;
        });
        return Arrays.copyOf(cachedResult, cachedResult.length);
    }
}
