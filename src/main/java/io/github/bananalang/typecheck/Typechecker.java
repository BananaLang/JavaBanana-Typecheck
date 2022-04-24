package io.github.bananalang.typecheck;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Deque;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.parse.ast.AccessExpression;
import io.github.bananalang.parse.ast.AssignmentExpression;
import io.github.bananalang.parse.ast.CallExpression;
import io.github.bananalang.parse.ast.ExpressionNode;
import io.github.bananalang.parse.ast.ExpressionStatement;
import io.github.bananalang.parse.ast.FunctionDefinitionStatement;
import io.github.bananalang.parse.ast.IdentifierExpression;
import io.github.bananalang.parse.ast.ImportStatement;
import io.github.bananalang.parse.ast.ReturnStatement;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.VariableDeclaration;
import io.github.bananalang.typecheck.Imported.ImportType;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtField;
import javassist.CtMethod;
import javassist.CtPrimitiveType;
import javassist.Modifier;
import javassist.NotFoundException;

public final class Typechecker {
    private static final Map<Map.Entry<String, String>, Boolean> cachedAssignableLookups = new HashMap<>();
    private static final CtClass CT_JLO, CT_JLS;
    private static final EvaluatedType ET_JLS, ET_VOID;

    static {
        try {
            ClassPool cp = ClassPool.getDefault();
            CT_JLO = cp.get("java.lang.Object");
            CT_JLS = cp.get("java.lang.String");
        } catch (NotFoundException e) {
            throw new Error(e);
        }
        ET_JLS = new EvaluatedType(CT_JLS);
        ET_VOID = new EvaluatedType(CtPrimitiveType.voidType);
    }

    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final Map<String, Imported<?>> imports = new HashMap<>();
    private final ClassPool cp;
    private final Map<StatementList, Map<String, LocalVariable>> scopes = new IdentityHashMap<>();
    private final Deque<StatementList> scopeStack = new ArrayDeque<>();
    private final Map<CallExpression, MethodCall> methodCalls = new IdentityHashMap<>();
    private final Map<FunctionDefinitionStatement, ScriptMethod> methodDefinitions = new IdentityHashMap<>();
    private final Map<String, List<ScriptMethod>> definedMethods = new HashMap<>();
    private EvaluatedType returnType = ET_VOID;
    private final List<EvaluatedType> potentialReturns = new ArrayList<>();
    private final List<LocalVariable> functionArgs = new ArrayList<>();
    private Map<String, LocalVariable> currentScope = null;

    public Typechecker(ClassPool cp) {
        this.cp = cp;
    }

    public Typechecker() {
        this(ClassPool.getDefault());
    }

    public void typecheck(StatementList root) {
        evaluateImports(root);
        evaluateMethodHeaders(root);
        typecheck0(root);
    }

    public EvaluatedType getType(ASTNode node) {
        return types.get(node);
    }

    public Map<StatementList, Map<String, LocalVariable>> getScopes() {
        return Collections.unmodifiableMap(scopes);
    }

    public MethodCall getMethodCall(CallExpression node) {
        return methodCalls.get(node);
    }

    public ScriptMethod getMethodDefinition(FunctionDefinitionStatement node) {
        return methodDefinitions.get(node);
    }

    private void typecheck0(ASTNode root) {
        if (root instanceof StatementList) {
            StatementList sl = (StatementList)root;
            scopeStack.addLast(sl);
            scopes.put(sl, currentScope = new LinkedHashMap<>());
            if (!functionArgs.isEmpty()) {
                for (LocalVariable arg : functionArgs) {
                    currentScope.put(arg.getName(), arg);
                }
                functionArgs.clear();
            }
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
                if (decl.value != null) {
                    EvaluatedType exprType = evaluateExpression(decl.value);
                    if (exprType.getName().equals("void")) {
                        throw new TypeCheckFailure("Cannot create void variable");
                    }
                    if (decl.type == null) {
                        declTypes[i] = exprType;
                    } else {
                        verifyTypeAssignable(declTypes[i] = evaluateTypeIdentifier(decl.type), exprType);
                    }
                } else {
                    declTypes[i] = evaluateTypeIdentifier(decl.type);
                }
                currentScope.put(decl.name, new LocalVariable(decl.name, declTypes[i], currentScope.size(), decl.value != null));
            }
        } else if (root instanceof FunctionDefinitionStatement) {
            FunctionDefinitionStatement functionDef = (FunctionDefinitionStatement)root;
            ScriptMethod method = methodDefinitions.get(functionDef);
            returnType = method.getReturnType();
            for (int i = 0; i < functionDef.args.length; i++) {
                functionArgs.add(new LocalVariable(functionDef.args[i].name, method.getArgTypes()[i], functionArgs.size(), true));
            }
            typecheck0(functionDef.body);
            if (returnType == null) {
                if (potentialReturns.isEmpty()) {
                    returnType = ET_VOID;
                } else {
                    for (EvaluatedType maybeReturnType : potentialReturns) {
                        if (returnType == null) {
                            returnType = maybeReturnType;
                            continue;
                        }
                        if (maybeReturnType.isAssignableTo(returnType)) {
                            continue;
                        }
                        if (!returnType.isAssignableTo(maybeReturnType)) {
                            throw new TypeCheckFailure("Incompatible return types: " + returnType.getName() + " and " + maybeReturnType.getName());
                        }
                        returnType = maybeReturnType;
                    }
                    potentialReturns.clear();
                }
                method.setReturnType(returnType);
            }
            returnType = ET_VOID;
        } else if (root instanceof ReturnStatement) {
            ReturnStatement returnStmt = (ReturnStatement)root;
            EvaluatedType checkType = returnStmt.value != null
                ? evaluateExpression(returnStmt.value)
                : ET_VOID;
            if (returnType == null) {
                potentialReturns.add(checkType);
            } else {
                verifyTypeAssignable(returnType, checkType);
            }
        } else if (!(root instanceof ImportStatement)) {
            throw new TypeCheckFailure("Typechecking of " + root.getClass().getSimpleName() + " not supported yet");
        }
    }

    private void evaluateMethodHeaders(StatementList root) {
        for (StatementNode stmt : root.children) {
            if (!(stmt instanceof FunctionDefinitionStatement)) continue;
            FunctionDefinitionStatement functionDef = (FunctionDefinitionStatement)stmt;
            EvaluatedType[] argTypes = new EvaluatedType[functionDef.args.length];
            for (int i = 0; i < argTypes.length; i++) {
                VariableDeclaration arg = functionDef.args[i];
                if (arg.type == null) {
                    argTypes[i] = evaluateExpression(arg.value);
                } else {
                    argTypes[i] = evaluateTypeIdentifier(arg.type);
                    if (arg.value != null) {
                        verifyTypeAssignable(argTypes[i], evaluateExpression(arg.value));
                    }
                }
            }
            ScriptMethod method = new ScriptMethod(functionDef.name, ifNotNull(functionDef.returnType, this::evaluateTypeIdentifier), argTypes);
            methodDefinitions.put(functionDef, method);
            definedMethods.computeIfAbsent(functionDef.name, k -> new ArrayList<>(1)).add(method);
        }
    }

    private void addImport(Imported<?> imported) {
        imports.put(imported.getShortName(), imported);
    }

    private void addImports(Collection<Imported<?>> imports) {
        for (Imported<?> imported : imports) {
            addImport(imported);
        }
    }

    private void evaluateImports(StatementList root) {
        addImport(Imported.class_(cp, "java.lang.Class"));
        addImport(Imported.class_(CT_JLO));
        addImport(Imported.class_(CT_JLS));
        try {
            addImports(Imported.starImport(cp, "banana.builtin.ModuleBuiltin"));
        } catch (TypeCheckFailure e) {
            // No stdlib installed
        }
        for (StatementNode stmt : root.children) {
            if (!(stmt instanceof ImportStatement)) {
                continue;
            }
            ImportStatement importStmt = (ImportStatement)stmt;
            addImports(Imported.infer(cp, importStmt.module, importStmt.name));
        }
    }

    private EvaluatedType evaluateTypeIdentifier(String identifier) {
        Imported<?> imported = imports.get(identifier);
        if (imported != null && imported.getType() == ImportType.CLASS) {
            @SuppressWarnings("unchecked")
            Imported<CtClass> classImport = (Imported<CtClass>)imported;
            return new EvaluatedType(classImport.getObject());
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
            MethodCall method = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                if (targetType.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot call method on void");
                }
                CtClass clazz = targetType.getJavassist();
                method = new MethodCall(findMethod(clazz, ae.name, true, false, argTypes));
            } else if (ce.target instanceof IdentifierExpression) {
                IdentifierExpression ie = (IdentifierExpression)ce.target;
                List<ScriptMethod> checkMethods = definedMethods.get(ie.identifier);
                if (checkMethods != null) {
                    searchDefinitionsLoop:
                    for (ScriptMethod checkMethod : checkMethods) {
                        EvaluatedType[] checkArgTypes = checkMethod.getArgTypes();
                        if (checkArgTypes.length != argTypes.length) continue;
                        for (int i = 0; i < checkArgTypes.length; i++) {
                            if (!checkTypeAssignable(checkArgTypes[i].getName(), argTypes[i])) {
                                continue searchDefinitionsLoop;
                            }
                        }
                        method = new MethodCall(checkMethod);
                        break;
                    }
                }
                if (method == null) {
                    Imported<?> imported = imports.get(ie.identifier);
                    if (imported != null && imported.getType() == ImportType.STATIC_METHOD) {
                        @SuppressWarnings("unchecked")
                        Imported<CtMethod> methodImport = (Imported<CtMethod>)imported;
                        try {
                            method = new MethodCall(findMethod(
                                methodImport.getOwnedClass().getDeclaredMethods(ie.identifier),
                                ie.identifier,
                                false,
                                true,
                                argTypes
                            ));
                        } catch (NotFoundException e) {
                            throw new TypeCheckFailure(e);
                        }
                    }
                }
            }
            if (method == null) {
                throw new TypeCheckFailure("Could not find method associated with " + ce);
            }
            methodCalls.put(ce, method);
            EvaluatedType returnType = method.getReturnType();
            if (returnType == null) {
                throw new TypeCheckFailure(
                    "Forward reference (or self-reference) to a function with an inferred return type"
                );
            }
            types.put(expr, returnType);
        } else if (expr instanceof IdentifierExpression) {
            IdentifierExpression ie = (IdentifierExpression)expr;
            EvaluatedType type = null;
            LocalVariable variable = evaluateVariable(ie.identifier);
            if (variable != null) {
                if (!variable.isAssigned()) {
                    throw new TypeCheckFailure("Variable " + variable + " accessed before it's assigned to");
                }
                type = variable.getType();
            }
            if (type == null) {
                Imported<?> imported = imports.get(ie.identifier);
                if (imported != null && imported.getType() == ImportType.STATIC_FIELD) {
                    @SuppressWarnings("unchecked")
                    CtField field = ((Imported<CtField>)imported).getObject();
                    try {
                        type = new EvaluatedType(field.getType());
                    } catch (NotFoundException e) {
                        throw new TypeCheckFailure(e);
                    }
                }
            }
            if (type == null) {
                throw new TypeCheckFailure("Could not find variable " + ie.identifier);
            }
            types.put(expr, type);
        } else if (expr instanceof AssignmentExpression) {
            AssignmentExpression assignExpr = (AssignmentExpression)expr;
            EvaluatedType valueType = evaluateExpression(assignExpr.value);
            if (assignExpr.target instanceof IdentifierExpression) {
                IdentifierExpression identifierExpr = (IdentifierExpression)assignExpr.target;
                LocalVariable variable = evaluateVariable(identifierExpr.identifier);
                if (variable == null) {
                    throw new TypeCheckFailure("Variable " + variable + " is not defined");
                }
                if (!checkTypeAssignable(variable.getType().getName(), valueType.getJavassist())) {
                    throw new TypeCheckFailure(
                        "Cannot assign expression of type " + valueType.getName() +
                        " to variable " + variable.getName() + " of type " + variable.getType().getName()
                    );
                }
                variable.setAssigned(true);
            } else {
                throw new TypeCheckFailure("Non-direct assignments not supported yet");
            }
            types.put(expr, valueType);
        } else if (expr instanceof StringExpression) {
            types.put(expr, ET_JLS);
        } else {
            throw new TypeCheckFailure("Typechecking of " + expr.getClass().getSimpleName() + " not supported yet");
        }
        return getType(expr);
    }

    private LocalVariable evaluateVariable(String name) {
        Iterator<StatementList> scopeIterator = scopeStack.descendingIterator();
        while (scopeIterator.hasNext()) {
            Map<String, LocalVariable> scope = scopes.get(scopeIterator.next());
            LocalVariable variable = scope.get(name);
            if (variable != null) {
                return variable;
            }
        }
        return null;
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
            throw new TypeCheckFailure(expr.getName() + " not assignable to " + assignTo.getName());
        }
    }

    static void verifyTypeAssignable(String assignTo, CtClass expr) {
        if (!checkTypeAssignable(assignTo, expr)) {
            throw new TypeCheckFailure(expr.getName() + " not assignable to " + assignTo);
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

    private static <T, R> R ifNotNull(T value, Function<T, R> ifNotNull) {
        return value == null ? null : ifNotNull.apply(value);
    }
}
