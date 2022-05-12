package io.github.bananalang.typecheck;

import javassist.CtClass;
import javassist.CtMethod;
import javassist.Modifier;
import javassist.NotFoundException;

public final class MethodCall {
    public static enum CallType {
        INSTANCE, STATIC, EXTENSION, FUNCTIONAL
    }

    public static final String EXTENSION_METHOD_ANNOTATION = "banana.internal.annotation.ExtensionMethod";

    private final ScriptMethod scriptMethod;
    private final CtMethod javaMethod;
    private final CallType callType;
    private EvaluatedType returnType = null;
    private EvaluatedType[] argTypes = null;
    private Boolean isStaticInvocation = null;

    public MethodCall(ScriptMethod method, CallType callMode) {
        scriptMethod = method;
        javaMethod = null;
        this.callType = callMode;
    }

    public MethodCall(CtMethod method, CallType callType) {
        scriptMethod = null;
        javaMethod = method;
        this.callType = callType;
    }

    public MethodCall(CtMethod method) {
        scriptMethod = null;
        javaMethod = method;
        this.callType =
            InformationLookup.hasAnnotation(method, EXTENSION_METHOD_ANNOTATION)
                ? CallType.EXTENSION
                : isStaticInvocation()
                    ? CallType.STATIC
                    : CallType.INSTANCE;
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

    public String getDescriptor() {
        if (isScriptMethod()) {
            StringBuilder descriptorBuilder = new StringBuilder("(");
            for (EvaluatedType argType : scriptMethod.getArgTypes()) {
                descriptorBuilder.append(argType.getDescriptor());
            }
            return descriptorBuilder.append(')')
                .append(scriptMethod.getReturnType().getDescriptor())
                .toString();
        } else {
            return javaMethod.getSignature();
        }
    }

    public CallType getCallType() {
        return callType;
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
