package io.github.bananalang.typecheck;

import javassist.CtClass;
import javassist.CtMethod;
import javassist.Modifier;
import javassist.NotFoundException;

public final class MethodCall {
    public static final String EXTENSION_METHOD_ANNOTATION = "banana.internal.annotation.ExtensionMethod";

    private final ScriptMethod scriptMethod;
    private final CtMethod javaMethod;
    private EvaluatedType returnType = null;
    private EvaluatedType[] argTypes = null;
    private Boolean isExtensionMethod = null;
    private Boolean isStaticInvocation = null;

    public MethodCall(ScriptMethod method) {
        scriptMethod = method;
        javaMethod = null;
    }

    public MethodCall(CtMethod method) {
        scriptMethod = null;
        javaMethod = method;
    }

    MethodCall(CtMethod method, Boolean isExtensionMethod) {
        this(method);
        this.isExtensionMethod = isExtensionMethod;
    }

    public ScriptMethod getScriptMethod() {
        return scriptMethod;
    }

    public CtMethod getJavaMethod() {
        return javaMethod;
    }

    public boolean isScriptMethod() {
        return scriptMethod != null;
    }

    public String getName() {
        return isScriptMethod() ? scriptMethod.getName() : javaMethod.getName();
    }

    public EvaluatedType getReturnType() {
        if (returnType != null) {
            return returnType;
        }
        try {
            return returnType = isScriptMethod() ? scriptMethod.getReturnType() : new EvaluatedType(javaMethod.getReturnType(), InformationLookup.isNullable(javaMethod));
        } catch (NotFoundException e) {
            throw new TypeCheckFailure(e);
        }
    }

    public EvaluatedType[] getArgTypes() {
        if (argTypes != null) {
            return argTypes;
        }
        if (isScriptMethod()) {
            return argTypes = scriptMethod.getArgTypes();
        } else {
            CtClass[] ctArgTypes;
            try {
                ctArgTypes = javaMethod.getParameterTypes();
            } catch (NotFoundException e) {
                throw new TypeCheckFailure(e);
            }
            boolean[] nullableAnnotation = InformationLookup.nullableParams(javaMethod);
            argTypes = new EvaluatedType[ctArgTypes.length];
            for (int i = 0; i < ctArgTypes.length; i++) {
                argTypes[i] = new EvaluatedType(ctArgTypes[i], nullableAnnotation[i]);
            }
            return argTypes;
        }
    }

    public boolean isExtensionMethod() {
        if (isExtensionMethod != null) {
            return isExtensionMethod;
        }
        if (isScriptMethod()) {
            return isExtensionMethod = scriptMethod.getModifiers().contains(Modifier2.EXTENSION);
        } else {
            return isExtensionMethod = javaMethod.hasAnnotation(EXTENSION_METHOD_ANNOTATION);
        }
    }

    public boolean isStaticInvocation() {
        if (isStaticInvocation != null) {
            return isStaticInvocation;
        }
        if (isScriptMethod()) {
            return isStaticInvocation = Boolean.TRUE;
        } else {
            return isStaticInvocation = Modifier.isStatic(javaMethod.getModifiers());
        }
    }
}
