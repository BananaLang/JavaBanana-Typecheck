package io.github.bananalang.typecheck;

import java.util.AbstractMap.SimpleImmutableEntry;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Deque;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.function.Function;
import java.util.function.Predicate;

import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.parse.ast.AccessExpression;
import io.github.bananalang.parse.ast.AssignmentExpression;
import io.github.bananalang.parse.ast.BinaryExpression;
import io.github.bananalang.parse.ast.CallExpression;
import io.github.bananalang.parse.ast.ExpressionNode;
import io.github.bananalang.parse.ast.ExpressionStatement;
import io.github.bananalang.parse.ast.FunctionDefinitionStatement;
import io.github.bananalang.parse.ast.IdentifierExpression;
import io.github.bananalang.parse.ast.IfOrWhileStatement;
import io.github.bananalang.parse.ast.ImportStatement;
import io.github.bananalang.parse.ast.NullExpression;
import io.github.bananalang.parse.ast.ReturnStatement;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.TypeReference;
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
    static final CtClass CT_JLO, CT_JLS;
    private static final EvaluatedType ET_JLS, ET_VOID;

    static {
        try {
            ClassPool cp = ClassPool.getDefault();
            CT_JLO = cp.get("java.lang.Object");
            CT_JLS = cp.get("java.lang.String");
        } catch (NotFoundException e) {
            throw new Error(e);
        }
        ET_JLS = new EvaluatedType(CT_JLS, false);
        ET_VOID = new EvaluatedType(CtPrimitiveType.voidType, false);
    }

    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final Map<String, Imported<?>> imports = new HashMap<>();
    private final ClassPool cp;
    private final Map<StatementList, Map<String, LocalVariable>> scopes = new IdentityHashMap<>();
    private final Deque<StatementList> scopeStack = new ArrayDeque<>();
    private final Map<ASTNode, MethodCall> methodCalls = new IdentityHashMap<>();
    private final Map<FunctionDefinitionStatement, ScriptMethod> methodDefinitions = new IdentityHashMap<>();
    private final Map<String, List<ScriptMethod>> definedMethods = new HashMap<>();
    private final Map<String, GlobalVariable> definedGlobals = new HashMap<>();
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
        evaluateGlobalsAndMethodHeaders(root);
        typecheck0(root);
    }

    public EvaluatedType getType(ASTNode node) {
        return types.get(node);
    }

    public Map<String, LocalVariable> getScope(StatementList owner) {
        return scopes.get(owner);
    }

    public MethodCall getMethodCall(ASTNode node) {
        return methodCalls.get(node);
    }

    public ScriptMethod getMethodDefinition(FunctionDefinitionStatement node) {
        return methodDefinitions.get(node);
    }

    public GlobalVariable getGlobalVariable(String name) {
        return definedGlobals.get(name);
    }

    private void typecheck0(ASTNode root) {
        if (root instanceof StatementList) {
            StatementList sl = (StatementList)root;
            scopeStack.addLast(sl);
            scopes.put(sl, currentScope = new HashMap<>());
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
            boolean isGlobalDecl = vds.isGlobalVariableDef();
            for (int i = 0; i < vds.declarations.length; i++) {
                VariableDeclaration decl = vds.declarations[i];
                if (!isGlobalDecl && currentScope.containsKey(decl.name)) {
                    throw new TypeCheckFailure("Duplicate variable " + decl.name);
                }
                GlobalVariable global = null;
                if (isGlobalDecl) {
                    global = definedGlobals.get(decl.name);
                    if (global == null) {
                        throw new TypeCheckFailure("Attempting to create local variable in illegal location");
                    }
                }
                if (decl.value != null) {
                    EvaluatedType exprType = evaluateExpression(decl.value);
                    if (exprType.getName().equals("void")) {
                        throw new TypeCheckFailure("Cannot create void variable");
                    }
                    if (decl.type == null) {
                        declTypes[i] = exprType;
                    } else if (isGlobalDecl) {
                        verifyTypeAssignable(global.getType(), exprType);
                    } else {
                        verifyTypeAssignable(declTypes[i] = evaluateType(decl.type), exprType);
                    }
                } else if (!isGlobalDecl) {
                    declTypes[i] = evaluateType(decl.type);
                }
                if (isGlobalDecl) {
                    if (global.getType() == null) {
                        global.setType(declTypes[i]);
                    }
                } else {
                    currentScope.put(decl.name, new LocalVariable(
                        decl.name,
                        declTypes[i],
                        currentScope.size(),
                        decl.value != null
                    ));
                }
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
                        if (returnType == null || returnType == EvaluatedType.NULL) {
                            returnType = maybeReturnType;
                            continue;
                        }
                        if (maybeReturnType == EvaluatedType.NULL) {
                            continue;
                        }
                        if (maybeReturnType.isAssignableTo(returnType)) {
                            continue;
                        }
                        if (!returnType.isAssignableTo(maybeReturnType)) {
                            throw new TypeCheckFailure("Incompatible return types: " + returnType + " and " + maybeReturnType);
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
        } else if (root instanceof IfOrWhileStatement) {
            IfOrWhileStatement ifOrWhileStmt = (IfOrWhileStatement)root;
            EvaluatedType conditionType = evaluateExpression(ifOrWhileStmt.condition);
            CtMethod method;
            if (conditionType == EvaluatedType.NULL) {
                method = null;
            } else {
                try {
                    method = conditionType.getJavassist().getMethod("truthy", "()Z");
                } catch (NotFoundException e) {
                    try {
                        method = conditionType.getJavassist().getMethod("isEmpty", "()Z");
                    } catch (NotFoundException e2) {
                        method = null; // Null check *only*
                        if (!conditionType.isNullable()) {
                            // TODO: make this a warning somehow
                        }
                    }
                }
            }
            methodCalls.put(ifOrWhileStmt, ifNotNull(method, MethodCall::new));
            typecheck0(ifOrWhileStmt.body);
            if (ifOrWhileStmt.elseBody != null) {
                typecheck0(ifOrWhileStmt.elseBody);
            }
        } else if (!(root instanceof ImportStatement)) {
            throw new TypeCheckFailure("Typechecking of " + root.getClass().getSimpleName() + " not supported yet");
        }
    }

    private void evaluateGlobalsAndMethodHeaders(StatementList root) {
        for (StatementNode stmt : root.children) {
            if (stmt instanceof FunctionDefinitionStatement) {
                FunctionDefinitionStatement functionDef = (FunctionDefinitionStatement)stmt;
                EvaluatedType[] argTypes = new EvaluatedType[functionDef.args.length];
                for (int i = 0; i < argTypes.length; i++) {
                    VariableDeclaration arg = functionDef.args[i];
                    if (arg.type == null) {
                        argTypes[i] = evaluateExpression(arg.value);
                    } else {
                        argTypes[i] = evaluateType(arg.type);
                        if (arg.value != null) {
                            verifyTypeAssignable(argTypes[i], evaluateExpression(arg.value));
                        }
                    }
                }
                ScriptMethod method = new ScriptMethod(
                    functionDef.name,
                    ifNotNull(functionDef.returnType, this::evaluateType),
                    argTypes
                );
                methodDefinitions.put(functionDef, method);
                definedMethods.computeIfAbsent(functionDef.name, k -> new ArrayList<>(1)).add(method);
            } else if (stmt instanceof VariableDeclarationStatement) {
                VariableDeclarationStatement varDef = (VariableDeclarationStatement)stmt;
                if (!varDef.isGlobalVariableDef()) {
                    continue;
                }
                for (VariableDeclaration decl : varDef.declarations) {
                    definedGlobals.put(decl.name, new GlobalVariable(
                        decl.name,
                        ifNotNull(decl.type, this::evaluateType),
                        varDef.modifiers
                    ));
                }
            }
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

    private EvaluatedType evaluateType(TypeReference type) {
        Imported<?> imported = imports.get(type.name);
        if (imported != null && imported.getType() == ImportType.CLASS) {
            @SuppressWarnings("unchecked")
            Imported<CtClass> classImport = (Imported<CtClass>)imported;
            return new EvaluatedType(classImport.getObject(), type.nullable);
        }
        throw new TypeCheckFailure("Could not find class " + type.name);
    }

    private EvaluatedType evaluateExpression(ExpressionNode expr) {
        if (expr instanceof CallExpression) {
            CallExpression ce = (CallExpression)expr;
            EvaluatedType[] argTypes = new EvaluatedType[ce.args.length];
            for (int i = 0; i < ce.args.length; i++) {
                EvaluatedType evaluated = evaluateExpression(ce.args[i]);
                if (evaluated.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot pass void as an argument to a function or method");
                }
                argTypes[i] = evaluated;
            }
            MethodCall method = null;
            EvaluatedType methodReturnType = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                if (ae.safeNavigation && !targetType.isNullable()) {
                    // TODO: make this a warning somehow
                    throw new TypeCheckFailure("Left-hand side of ?. must be nullable");
                }
                if (!ae.safeNavigation && targetType.isNullable()) {
                    throw new TypeCheckFailure("Left-hand side of . cannot be nullable. Did you mean to use ?. ?");
                }
                if (targetType.getName().equals("void")) {
                    throw new TypeCheckFailure("Cannot call method on void");
                }
                if (targetType == EvaluatedType.NULL) {
                    throw new TypeCheckFailure("Cannot call methods on literal null");
                }
                CtClass clazz = targetType.getJavassist();
                method = new MethodCall(findMethod(clazz, ae.name, true, false, argTypes));
                methodReturnType = method.getReturnType().nullable(ae.safeNavigation || method.getReturnType().isNullable());
            } else if (ce.target instanceof IdentifierExpression) {
                IdentifierExpression ie = (IdentifierExpression)ce.target;
                List<ScriptMethod> checkMethods = definedMethods.get(ie.identifier);
                if (checkMethods != null) {
                    searchDefinitionsLoop:
                    for (ScriptMethod checkMethod : checkMethods) {
                        EvaluatedType[] checkArgTypes = checkMethod.getArgTypes();
                        if (checkArgTypes.length != argTypes.length) continue;
                        for (int i = 0; i < checkArgTypes.length; i++) {
                            if (argTypes[i].isNullable() && !checkArgTypes[i].isNullable()) {
                                continue searchDefinitionsLoop;
                            }
                            if (!checkTypeAssignable(checkArgTypes[i].getName(), argTypes[i].getJavassist())) {
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
                if (method != null) {
                    methodReturnType = method.getReturnType();
                }
            }
            if (method == null) {
                StringBuilder error = new StringBuilder("Could not find method compatible with ");
                if (ce.target instanceof AccessExpression) {
                    AccessExpression ae = (AccessExpression)ce.target;
                    try {
                        error.append(evaluateExpression(ae.target));
                    } catch (TypeCheckFailure e) {
                        error.append("UNKNOWN");
                    }
                    error.append('.').append(ae.name);
                } else if (ce.target instanceof IdentifierExpression) {
                    error.append(((IdentifierExpression)ce.target).identifier);
                }
                error.append('(');
                for (int i = 0; i < argTypes.length; i++) {
                    if (i > 0) {
                        error.append(", ");
                    }
                    error.append(argTypes[i]);
                }
                throw new TypeCheckFailure(error.append(')').toString());
            }
            methodCalls.put(ce, method);
            if (methodReturnType == null) {
                throw new TypeCheckFailure(
                    "Forward reference (or self-reference) to a function with an inferred return type"
                );
            }
            types.put(expr, methodReturnType);
        } else if (expr instanceof IdentifierExpression) {
            IdentifierExpression ie = (IdentifierExpression)expr;
            EvaluatedType type = null;
            {
                LocalVariable variable = evaluateVariable(ie.identifier);
                if (variable != null) {
                    if (!variable.isAssigned()) {
                        throw new TypeCheckFailure("Variable " + variable.getName() + " accessed before it's assigned to");
                    }
                    type = variable.getType();
                }
            }
            if (type == null) {
                GlobalVariable variable = definedGlobals.get(ie.identifier);
                if (variable != null) {
                    type = variable.getType();
                    if (type == null) {
                        throw new TypeCheckFailure(
                            "Forward reference to a global variable with an inferred type"
                        );
                    }
                }
            }
            if (type == null) {
                Imported<?> imported = imports.get(ie.identifier);
                if (imported != null && imported.getType() == ImportType.STATIC_FIELD) {
                    @SuppressWarnings("unchecked")
                    CtField field = ((Imported<CtField>)imported).getObject();
                    try {
                        type = new EvaluatedType(field.getType(), NullableLookup.isNullable(field));
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
                if (!valueType.isAssignableTo(variable.getType())) {
                    throw new TypeCheckFailure(
                        "Cannot assign expression of type " + valueType +
                        " to variable " + variable.getName() + " of type " + variable.getType()
                    );
                }
                variable.setAssigned(true);
            } else {
                throw new TypeCheckFailure("Non-direct assignments not supported yet");
            }
            types.put(expr, valueType);
        } else if (expr instanceof BinaryExpression) {
            BinaryExpression binExpr = (BinaryExpression)expr;
            EvaluatedType leftType = evaluateExpression(binExpr.left);
            EvaluatedType rightType = evaluateExpression(binExpr.right);
            switch (binExpr.type) {
                case NULL_COALESCE: {
                    if (!leftType.isNullable()) {
                        // TODO: make this a warning somehow
                        throw new TypeCheckFailure("Left-hand-side of ?? must be nullable");
                    }
                    if (rightType == EvaluatedType.NULL) {
                        throw new TypeCheckFailure("Right-hand side of ?? cannot be literal null");
                    }
                    EvaluatedType moreGeneral = rightType.isAssignableTo(leftType) ? leftType : rightType;
                    types.put(expr, moreGeneral.nullable(rightType.isNullable()));
                    break;
                }
                default:
                    throw new TypeCheckFailure("Typechecking of " + binExpr.type + " operator not supported yet");
            }
        } else if (expr instanceof NullExpression) {
            types.put(expr, EvaluatedType.NULL);
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

    private static CtMethod findMethod(CtMethod[] methods, String name, boolean checkName, boolean staticOnly, EvaluatedType... argTypes) {
        try {
            methodCheckLoop:
            for (CtMethod maybe : methods) {
                if (checkName && !maybe.getName().equals(name)) continue;
                if (!isAccessible(maybe)) continue;
                if (staticOnly && !Modifier.isStatic(maybe.getModifiers())) continue;
                CtClass[] methodParamTypes = maybe.getParameterTypes();
                boolean[] nullableParams = NullableLookup.nullableParams(maybe);
                if (Modifier.isVarArgs(maybe.getModifiers())) {
                    if (argTypes.length < methodParamTypes.length - 1) continue;
                    for (int i = 0; i < methodParamTypes.length - 1; i++) {
                        if (argTypes[i].isNullable() && !nullableParams[i]) {
                            continue methodCheckLoop;
                        }
                        if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i].getJavassist())) {
                            continue methodCheckLoop;
                        }
                    }
                    boolean areVarargsNullable = nullableParams[methodParamTypes.length - 1];
                    String varargType = methodParamTypes[methodParamTypes.length - 1].getComponentType().getName();
                    for (int i = methodParamTypes.length - 1; i < argTypes.length; i++) {
                        if (argTypes[i].isNullable() && !areVarargsNullable) {
                            continue methodCheckLoop;
                        }
                        if (!checkTypeAssignable(varargType, argTypes[i].getJavassist())) {
                            continue methodCheckLoop;
                        }
                    }
                } else {
                    if (methodParamTypes.length != argTypes.length) continue;
                    for (int i = 0; i < methodParamTypes.length; i++) {
                        if (argTypes[i].isNullable() && !nullableParams[i]) {
                            continue methodCheckLoop;
                        }
                        if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i].getJavassist())) {
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

    private static CtMethod findMethod(CtClass clazz, String name, boolean checkName, boolean staticOnly, EvaluatedType... argTypes) {
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
            throw new TypeCheckFailure(expr.getName() + " not assignable to " + assignTo);
        }
    }

    static boolean checkTypeAssignable(String assignTo, CtClass expr) {
        if (expr == null) {
            // null is assignable to everything. At this point we've already done nullness checks.
            return true;
        }
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
