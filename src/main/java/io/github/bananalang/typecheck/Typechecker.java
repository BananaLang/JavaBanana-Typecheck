package io.github.bananalang.typecheck;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Deque;
import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Predicate;

import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.parse.ast.AccessExpression;
import io.github.bananalang.parse.ast.CallExpression;
import io.github.bananalang.parse.ast.ExpressionNode;
import io.github.bananalang.parse.ast.ExpressionStatement;
import io.github.bananalang.parse.ast.IdentifierExpression;
import io.github.bananalang.parse.ast.ImportStatement;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.VariableDeclaration;
import io.github.bananalang.typecheck.Imported.ImportType;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.Modifier;
import javassist.NotFoundException;

public final class Typechecker {
    private static final Map<Map.Entry<String, String>, Boolean> cachedAssignableLookups = new HashMap<>();
    private static final CtClass CT_JLO, CT_JLS;

    static {
        try {
            ClassPool cp = ClassPool.getDefault();
            CT_JLO = cp.get("java.lang.Object");
            CT_JLS = cp.get("java.lang.String");
        } catch (NotFoundException e) {
            throw new Error(e);
        }
    }

    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final List<Imported<?>> imports = new ArrayList<>();
    private final ClassPool cp;
    private final Map<StatementList, Map<String, EvaluatedType>> scopes = new IdentityHashMap<>();
    private final Deque<StatementList> scopeStack = new ArrayDeque<>();
    private final Map<CallExpression, CtMethod> methodCalls = new IdentityHashMap<>();
    private Map<String, EvaluatedType> currentScope = null;

    public Typechecker(ClassPool cp) {
        this.cp = cp;
    }

    public Typechecker() {
        this(ClassPool.getDefault());
    }

    public EvaluatedType typecheck(StatementList root) {
        evaluateImports(root);
        return typecheck0(root);
    }

    public EvaluatedType getType(ASTNode node) {
        return types.get(node);
    }

    public Map<StatementList, Map<String, EvaluatedType>> getScopes() {
        return Collections.unmodifiableMap(scopes);
    }

    public CtMethod getMethodCall(CallExpression node) {
        return methodCalls.get(node);
    }

    private EvaluatedType typecheck0(ASTNode root) {
        if (root instanceof StatementList) {
            StatementList sl = (StatementList)root;
            scopeStack.addLast(sl);
            scopes.put(sl, currentScope = new LinkedHashMap<>());
            for (StatementNode stmt : sl.children) {
                typecheck0(stmt);
            }
            scopeStack.removeLast();
            currentScope = scopeStack.isEmpty() ? null : scopes.get(scopeStack.peekLast());
        } else if (root instanceof ExpressionStatement) {
            ExpressionStatement es = (ExpressionStatement)root;
            evaluateExpression(es.expression);
        } else if (root instanceof VariableDeclarationStatement) {
            VariableDeclarationStatement vds = (VariableDeclarationStatement)root;
            EvaluatedType[] declTypes = new EvaluatedType[vds.declarations.length];
            for (int i = 0; i < vds.declarations.length; i++) {
                VariableDeclaration decl = vds.declarations[i];
                if (currentScope.containsKey(decl.name)) {
                    throw new TypeCheckFailure("Duplicate variable " + decl.name);
                }
                EvaluatedType exprType = evaluateExpression(decl.value);
                if (exprType.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot create void variable");
                }
                if (decl.type == null) {
                    declTypes[i] = exprType;
                } else {
                    verifyTypeAssignable(declTypes[i] = evaluateTypeIdentifier(decl.type), exprType);
                }
                currentScope.put(decl.name, declTypes[i]);
            }
        } else if (!(root instanceof ImportStatement)) {
            throw new TypeCheckFailure("Typechecking of " + root.getClass().getSimpleName() + " not supported yet");
        }
        return getType(root);
    }

    private void evaluateImports(StatementList root) {
        imports.add(Imported.class_(cp, "java.lang.Class"));
        imports.add(Imported.class_(CT_JLO));
        imports.add(Imported.class_(CT_JLS));
        try {
            imports.addAll(Imported.starImport(cp, "banana.builtin.ModuleBuiltin"));
        } catch (TypeCheckFailure e) {
            // No stdlib installed
        }
        for (StatementNode stmt : root.children) {
            if (!(stmt instanceof ImportStatement)) {
                continue;
            }
            ImportStatement importStmt = (ImportStatement)stmt;
            imports.addAll(Imported.infer(cp, importStmt.module, importStmt.name));
        }
    }

    private EvaluatedType evaluateTypeIdentifier(String identifier) {
        for (Imported<?> imported : imports) {
            if (imported.getType() == ImportType.CLASS && imported.getShortName().equals(identifier)) {
                @SuppressWarnings("unchecked")
                Imported<CtClass> classImport = (Imported<CtClass>)imported;
                return new EvaluatedType(classImport.getObject());
            }
        }
        throw new TypeCheckFailure("Could not find class " + identifier);
    }

    private EvaluatedType evaluateExpression(ExpressionNode expr) {
        if (expr instanceof CallExpression) {
            CallExpression ce = (CallExpression)expr;
            CtClass[] argTypes = new CtClass[ce.args.length];
            for (int i = 0; i < ce.args.length; i++) {
                EvaluatedType evaluated = evaluateExpression(ce.args[i]);
                if (evaluated.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot pass void as an argument to a function or method");
                }
                argTypes[i] = evaluated.getJavassist();
            }
            CtMethod method = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                if (targetType.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot call method on void");
                }
                CtClass clazz = targetType.getJavassist();
                method = findMethod(clazz, ae.name, true, false, argTypes);
            } else if (ce.target instanceof IdentifierExpression) {
                IdentifierExpression ie = (IdentifierExpression)ce.target;
                for (Imported<?> imported : imports) {
                    if (imported.getType() != ImportType.STATIC_METHOD) continue;
                    @SuppressWarnings("unchecked")
                    Imported<CtMethod> methodImport = (Imported<CtMethod>)imported;
                    if (methodImport.getShortName().equals(ie.identifier)) {
                        try {
                            method = findMethod(methodImport.getOwnedClass().getDeclaredMethods(ie.identifier), ie.identifier, false, true, argTypes);
                        } catch (NotFoundException e) {
                            throw new TypeCheckFailure(e);
                        }
                        break;
                    }
                }
            }
            if (method == null) {
                throw new TypeCheckFailure("Could not find method associated with " + ce);
            }
            methodCalls.put(ce, method);
            try {
                types.put(expr, new EvaluatedType(method.getReturnType()));
            } catch (NotFoundException e) {
                throw new TypeCheckFailure(e);
            }
        } else if (expr instanceof IdentifierExpression) {
            IdentifierExpression ie = (IdentifierExpression)expr;
            EvaluatedType type = null;
            Iterator<StatementList> scopeIterator = scopeStack.descendingIterator();
            while (scopeIterator.hasNext()) {
                Map<String, EvaluatedType> scope = scopes.get(scopeIterator.next());
                type = scope.get(ie.identifier);
                if (type != null) {
                    break;
                }
            }
            if (type == null) {
                throw new TypeCheckFailure("Could not find variable " + ie.identifier);
            }
            types.put(expr, type);
        } else if (expr instanceof StringExpression) {
            types.put(expr, new EvaluatedType(CT_JLS));
        } else {
            throw new TypeCheckFailure("Typechecking of " + expr.getClass().getSimpleName() + " not supported yet");
        }
        return getType(expr);
    }

    private static CtMethod findMethod(CtMethod[] methods, String name, boolean checkName, boolean staticOnly, CtClass... argTypes) {
        try {
            methodCheckLoop:
            for (CtMethod maybe : methods) {
                if (checkName && !maybe.getName().equals(name)) continue;
                if (!isAccessible(maybe)) continue;
                if (staticOnly && !Modifier.isStatic(maybe.getModifiers())) continue;
                CtClass[] methodParamTypes = maybe.getParameterTypes();
                if (Modifier.isVarArgs(maybe.getModifiers())) {
                    if (argTypes.length < methodParamTypes.length - 1) continue;
                    for (int i = 0; i < methodParamTypes.length - 1; i++) {
                        if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i])) {
                            continue methodCheckLoop;
                        }
                    }
                    String varargType = methodParamTypes[methodParamTypes.length - 1].getComponentType().getName();
                    for (int i = methodParamTypes.length - 1; i < argTypes.length; i++) {
                        if (!checkTypeAssignable(varargType, argTypes[i])) {
                            continue methodCheckLoop;
                        }
                    }
                } else {
                    if (methodParamTypes.length != argTypes.length) continue;
                    for (int i = 0; i < methodParamTypes.length; i++) {
                        if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i])) {
                            continue methodCheckLoop;
                        }
                    }
                }
                return maybe;
            }
        } catch (NotFoundException e) {
            throw new TypeCheckFailure(e);
        }
        StringBuilder error = new StringBuilder("Unable to find method ").append(name).append('(');
        for (int i = 0; i < argTypes.length; i++) {
            if (i > 0) {
                error.append(", ");
            }
            error.append(argTypes[i].getName());
        }
        error.append(')');
        throw new TypeCheckFailure(error.toString());
    }

    private static CtMethod findMethod(CtClass clazz, String name, boolean checkName, boolean staticOnly, CtClass... argTypes) {
        return findMethod(clazz.getMethods(), name, checkName, staticOnly, argTypes);
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
            try {
                CtClass superclass = tryClass.getSuperclass();
                if (superclass == null) {
                    // We've reached java.lang.Object or a primitve, so this is the end of the chain. Return early.
                    return false;
                }
                if (!superclass.getName().equals("java.lang.Object")) {
                    toSearch.add(superclass);
                }
            } catch (NotFoundException e) {
                throw new TypeCheckFailure(e);
            }
            try {
                for (CtClass intf : tryClass.getInterfaces()) {
                    toSearch.add(intf);
                }
            } catch (NotFoundException e) {
                throw new TypeCheckFailure(e);
            }
        }
        return action.test(CT_JLO);
    }

    static void verifyTypeAssignable(EvaluatedType assignTo, EvaluatedType expr) {
        if (!expr.isAssignableTo(assignTo)) {
            throw new TypeCheckFailure(expr + " not assignable to " + assignTo);
        }
    }

    static void verifyTypeAssignable(String assignTo, CtClass expr) {
        if (!checkTypeAssignable(assignTo, expr)) {
            throw new TypeCheckFailure(expr + " not assignable to " + assignTo);
        }
    }

    static boolean checkTypeAssignable(String assignTo, CtClass expr) {
        if (assignTo.equals(expr.getName())) {
            return true; // Fast path
        }
        return cachedAssignableLookups.computeIfAbsent(new SimpleImmutableEntry<>(assignTo, expr.getName()), k ->
            forEachSuperclass(expr, clazz -> clazz.getName().equals(assignTo))
        );
    }
}
