package io.github.bananalang.typecheck;

import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;

public final class MethodCall {
    private final ScriptMethod scriptMethod;
    private final CtMethod javaMethod;
    private EvaluatedType returnType = null;
    private EvaluatedType[] argTypes = null;

    public MethodCall(ScriptMethod method) {
        scriptMethod = method;
        javaMethod = null;
    }

    public MethodCall(CtMethod method) {
        scriptMethod = null;
        javaMethod = method;
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

    public EvaluatedType getReturnType() {
        if (returnType != null) {
            return returnType;
        }
        try {
            return returnType = isScriptMethod() ? scriptMethod.getReturnType() : new EvaluatedType(javaMethod.getReturnType());
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
            argTypes = new EvaluatedType[ctArgTypes.length];
            for (int i = 0; i < ctArgTypes.length; i++) {
                argTypes[i] = new EvaluatedType(ctArgTypes[i]);
            }
            return argTypes;
        }
    }
}
