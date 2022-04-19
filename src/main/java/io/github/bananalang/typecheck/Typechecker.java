package io.github.bananalang.typecheck;

import java.lang.reflect.Modifier;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Deque;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.parse.ast.AccessExpression;
import io.github.bananalang.parse.ast.CallExpression;
import io.github.bananalang.parse.ast.ExpressionNode;
import io.github.bananalang.parse.ast.ExpressionStatement;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.VariableDeclaration;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;

public final class Typechecker {
    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final List<ImportedName> imports = new ArrayList<>();
    private final ClassPool cp = ClassPool.getDefault();

    {
        imports.add(ImportedName.className("java.lang.Class"));
        imports.add(ImportedName.className("java.lang.Object"));
        imports.add(ImportedName.className("java.lang.String"));
    }

    public Typechecker() {
    }

    public EvaluatedType typecheck(ASTNode root) {
        if (root instanceof StatementList) {
            StatementList sl = (StatementList)root;
            for (StatementNode stmt : sl.children) {
                typecheck(stmt);
            }
        } else if (root instanceof ExpressionStatement) {
            ExpressionStatement es = (ExpressionStatement)root;
            typecheck(es.expression);
        } else if (root instanceof VariableDeclarationStatement) {
            VariableDeclarationStatement vds = (VariableDeclarationStatement)root;
            EvaluatedType[] declTypes = new EvaluatedType[vds.declarations.length];
            for (int i = 0; i < vds.declarations.length; i++) {
                VariableDeclaration decl = vds.declarations[i];
                EvaluatedType exprType = evaluateExpression(decl.value);
                if (decl.type == null) {
                    declTypes[i] = exprType;
                } else {
                    verifyTypeAssignable(declTypes[i] = evaluateIdentifier(decl.type), exprType);
                }
            }
        } else {
            throw new IllegalArgumentException("Typechecking of " + root.getClass().getSimpleName() + " not supported yet");
        }
        return getType(root);
    }

    public EvaluatedType getType(ASTNode node) {
        return types.get(node);
    }

    private EvaluatedType evaluateIdentifier(String identifier) {
        for (ImportedName imported : imports) {
            if (imported.getClassName().equals(identifier)) {
                return new EvaluatedType(cp, imported.getQualName());
            }
        }
        throw new IllegalArgumentException("Could not find class " + identifier);
    }

    private EvaluatedType evaluateExpression(ExpressionNode expr) {
        if (expr instanceof CallExpression) {
            CallExpression ce = (CallExpression)expr;
            CtMethod method = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                CtClass clazz = targetType.getJavassist();
                CtClass[] argTypes = new CtClass[ce.args.length];
                for (int i = 0; i < ce.args.length; i++) {
                    argTypes[i] = evaluateExpression(ce.args[i]).getJavassist();
                }
                method = findMethod(clazz, ae.name, argTypes);
            }
            if (method == null) {
                throw new IllegalArgumentException("Could not find method associated with " + ce);
            }
            try {
                types.put(expr, new EvaluatedType(method.getReturnType()));
            } catch (NotFoundException e) {
                throw new IllegalArgumentException(e);
            }
        } else if (expr instanceof StringExpression) {
            types.put(expr, new EvaluatedType(cp, "java.lang.String"));
        } else {
            throw new IllegalArgumentException("Typechecking of " + expr.getClass().getSimpleName() + " not supported yet");
        }
        return getType(expr);
    }

    private static CtMethod findMethod(CtClass clazz, String name, CtClass... argTypes) {
        CtMethod[] result = new CtMethod[1];
        if (!forEachSuperclass(clazz, superClazz -> {
            try {
                methodCheckLoop:
                for (CtMethod maybe : clazz.getDeclaredMethods(name)) {
                    if (!isAccessible(maybe)) continue;
                    CtClass[] methodParamTypes = maybe.getParameterTypes();
                    if (methodParamTypes.length != argTypes.length) continue;
                    for (int i = 0; i < methodParamTypes.length; i++) {
                        if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i])) {
                            continue methodCheckLoop;
                        }
                    }
                    result[0] = maybe;
                    return true;
                }
            } catch (NotFoundException e) {
                throw new IllegalArgumentException(e);
            }
            return false;
        })) {
            String argStr = Arrays.toString(argTypes);
            throw new IllegalArgumentException("Unable to find method " + name + "(" + argStr.substring(1, argStr.length() - 1) + ")");
        }
        return result[0];
    }

    private static boolean isAccessible(CtMethod method) {
        return Modifier.isPublic(method.getModifiers());
    }

    static boolean forEachSuperclass(CtClass clazz, Predicate<CtClass> action) {
        Deque<CtClass> toSearch = new ArrayDeque<>();
        toSearch.add(clazz);
        while (!toSearch.isEmpty()) {
            CtClass tryClass = toSearch.poll();
            if (action.test(tryClass)) {
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

    static void verifyTypeAssignable(EvaluatedType assignTo, EvaluatedType expr) {
        if (!expr.isAssignableTo(assignTo)) {
            throw new IllegalArgumentException(expr + " not assignable to " + assignTo);
        }
    }

    static void verifyTypeAssignable(String assignTo, CtClass expr) {
        if (!checkTypeAssignable(assignTo, expr)) {
            throw new IllegalArgumentException(expr + " not assignable to " + assignTo);
        }
    }

    static boolean checkTypeAssignable(String assignTo, CtClass expr) {
        return forEachSuperclass(expr, clazz -> clazz.getName().equals(assignTo));
    }
}
