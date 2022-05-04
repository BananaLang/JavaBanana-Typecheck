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

import io.github.bananalang.JavaBananaConstants;
import io.github.bananalang.compilecommon.problems.ProblemCollector;
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
import io.github.bananalang.parse.ast.ReservedIdentifierExpression;
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

    private final ClassPool cp;
    private final ProblemCollector problemCollector;
    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final Map<String, Imported<?>> imports = new HashMap<>();
    private final Map<StatementList, Map<String, LocalVariable>> scopes = new IdentityHashMap<>();
    private final Deque<StatementList> scopeStack = new ArrayDeque<>();
    private final Map<ASTNode, MethodCall> methodCalls = new IdentityHashMap<>();
    private final Map<ExpressionNode, CtField> fieldAccesses = new IdentityHashMap<>();
    private final Map<FunctionDefinitionStatement, ScriptMethod> methodDefinitions = new IdentityHashMap<>();
    private final Map<String, List<ScriptMethod>> definedMethods = new HashMap<>();
    private final Map<String, GlobalVariable> definedGlobals = new HashMap<>();
    private EvaluatedType returnType = ET_VOID;
    private final List<EvaluatedType> potentialReturns = new ArrayList<>();
    private final List<LocalVariable> functionArgs = new ArrayList<>();
    private Map<String, LocalVariable> currentScope = null;

    public Typechecker(ClassPool cp, ProblemCollector problemCollector) {
        this.cp = cp;
        this.problemCollector = problemCollector;
    }

    public Typechecker(ProblemCollector problemCollector) {
        this(ClassPool.getDefault(), problemCollector);
    }

    public void typecheck(StatementList root) {
        double startTime = System.nanoTime();
        if (JavaBananaConstants.DEBUG) {
            System.out.println("Beginning typecheck of 0x" + Integer.toHexString(root.hashCode()));
        }
        try {
            evaluateImports(root);
            evaluateGlobalsAndMethodHeaders(root);
            typecheck0(root);
        } catch (TypeCheckFailure e) {
            problemCollector.error(e.getMessage(), e.row, e.column);
        }
        if (JavaBananaConstants.DEBUG) {
            System.out.println("Finished typecheck in " + (System.nanoTime() - startTime) / 1_000_000D + "ms");
        }
        problemCollector.throwIfFailing();
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

    public CtField getFieldAccess(ExpressionNode expr) {
        return fieldAccesses.get(expr);
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
                    error("Duplicate variable", root);
                }
                GlobalVariable global = null;
                if (isGlobalDecl) {
                    global = definedGlobals.get(decl.name);
                    if (global == null) {
                        error("Attempting to create local variable in illegal location", root);
                    }
                }
                if (decl.value != null) {
                    EvaluatedType exprType = evaluateExpression(decl.value);
                    if (exprType.getName().equals("void")) {
                        error("Cannot create void variable", root);
                    }
                    if (decl.type == null) {
                        declTypes[i] = exprType;
                    } else if (isGlobalDecl) {
                        verifyTypeAssignable(global.getType(), exprType, problemCollector);
                    } else {
                        verifyTypeAssignable(declTypes[i] = evaluateType(decl.type), exprType, problemCollector);
                    }
                } else if (!isGlobalDecl) {
                    declTypes[i] = evaluateType(decl.type);
                }
                if (isGlobalDecl) {
                    if (global.getType() == null) {
                        global.setType(declTypes[i].nullable(true));
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
                            error("Incompatible return types: " + returnType + " and " + maybeReturnType, root);
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
                verifyTypeAssignable(returnType, checkType, problemCollector);
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
                            warning(
                                "Condition does not support truthiness testing, nor is it nullable. " +
                                (ifOrWhileStmt.isWhile
                                    ? "This while statement will never exit unless break is used."
                                    : "This if statement will be inlined."),
                                ifOrWhileStmt.condition
                            );
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
            error("Typechecking of " + root.getClass().getSimpleName() + " not supported yet", root);
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
                            verifyTypeAssignable(argTypes[i], evaluateExpression(arg.value), problemCollector);
                        }
                    }
                }
                ScriptMethod method = new ScriptMethod(
                    functionDef.name,
                    ifNotNull(functionDef.returnType, this::evaluateType),
                    argTypes,
                    functionDef.modifiers
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
                        ifNotNull(decl.type, t -> evaluateType(t).nullable(true)),
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
                    error("Cannot pass void as an argument to a function or method", expr);
                }
                argTypes[i] = evaluated;
            }
            MethodCall method = null;
            EvaluatedType methodReturnType = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                if (ae.safeNavigation && !targetType.isNullable()) {
                    warning("Left-hand side of ?. must be nullable", ae);
                }
                if (!ae.safeNavigation && targetType.isNullable()) {
                    error("Left-hand side of . cannot be nullable. Did you mean to use ?. ?", ae);
                }
                if (targetType.getName().equals("void")) {
                    error("Cannot call method on void", ce);
                }
                if (targetType == EvaluatedType.NULL) {
                    error("Cannot call methods on literal null", ce);
                }
                CtClass clazz = targetType.getJavassist();
                {
                    EvaluatedType[] lookupTypes = new EvaluatedType[argTypes.length + 1];
                    lookupTypes[0] = targetType;
                    System.arraycopy(argTypes, 0, lookupTypes, 1, argTypes.length);
                    method = lookupStaticMethod(ae.name, lookupTypes, true);
                }
                if (method == null) {
                    method = new MethodCall(findMethod(clazz, ae.name, true, false, null, argTypes));
                }
                methodReturnType = method.getReturnType().nullable(ae.safeNavigation || method.getReturnType().isNullable());
            } else if (ce.target instanceof IdentifierExpression) {
                IdentifierExpression ie = (IdentifierExpression)ce.target;
                method = lookupStaticMethod(ie.identifier, argTypes, false);
                if (method != null) {
                    methodReturnType = method.getReturnType();
                }
            }
            if (method == null) {
                StringBuilder error = new StringBuilder("Could not find method compatible with ");
                if (ce.target instanceof AccessExpression) {
                    AccessExpression ae = (AccessExpression)ce.target;
                    try {
                        error.append(getType(ae.target));
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
                error(error.append(')').toString(), ce);
                methodReturnType = EvaluatedType.NULL;
            }
            methodCalls.put(ce, method);
            if (methodReturnType == null) {
                throw new TypeCheckFailure(
                    "Forward reference (or self-reference) to a function with an inferred return type",
                    ce
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
                        error("Variable " + variable.getName() + " accessed before it's assigned to", ie);
                    }
                    type = variable.getType();
                }
            }
            if (type == null) {
                GlobalVariable variable = definedGlobals.get(ie.identifier);
                if (variable != null) {
                    type = variable.getType();
                    if (type == null) {
                        error(
                            "Forward reference to a global variable with an inferred type", ie
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
                    fieldAccesses.put(ie, field);
                }
            }
            if (type == null) {
                error("Could not find variable " + ie.identifier, ie);
                type = EvaluatedType.NULL;
            }
            types.put(expr, type);
        } else if (expr instanceof AssignmentExpression) {
            AssignmentExpression assignExpr = (AssignmentExpression)expr;
            EvaluatedType valueType = evaluateExpression(assignExpr.value);
            if (assignExpr.target instanceof IdentifierExpression) {
                IdentifierExpression identifierExpr = (IdentifierExpression)assignExpr.target;
                LocalVariable local = evaluateVariable(identifierExpr.identifier);
                if (local != null) {
                    if (!valueType.isAssignableTo(local.getType())) {
                        error(
                            "Cannot assign expression of type " + valueType +
                            " to variable " + local.getName() + " of type " + local.getType(),
                            assignExpr
                        );
                    }
                    local.setAssigned(true);
                } else {
                    GlobalVariable global = definedGlobals.get(identifierExpr.identifier);
                    if (global != null) {
                        if (global.getType() == null) {
                            error(
                                "Forward reference to a global variable with an inferred type", assignExpr
                            );
                        }
                        if (!valueType.isAssignableTo(global.getType())) {
                            error(
                                "Cannot assign expression of type " + valueType +
                                " to global variable " + global.getName() + " of type " + global.getType(),
                                assignExpr
                            );
                        }
                    } else {
                        error("Variable " + identifierExpr.identifier + " is not defined", identifierExpr);
                    }
                }
            } else {
                error("Non-direct assignments not supported yet", assignExpr);
            }
            types.put(expr, valueType);
        } else if (expr instanceof BinaryExpression) {
            BinaryExpression binExpr = (BinaryExpression)expr;
            EvaluatedType leftType = evaluateExpression(binExpr.left);
            EvaluatedType rightType = evaluateExpression(binExpr.right);
            switch (binExpr.type) {
                case NULL_COALESCE: {
                    if (!leftType.isNullable()) {
                        warning("Left-hand-side of ?? isn't nullable", binExpr);
                    } else if (leftType == EvaluatedType.NULL) {
                        warning("Left-hand-side of ?? is literal null", binExpr);
                    }
                    if (rightType == EvaluatedType.NULL) {
                        error("Right-hand side of ?? cannot be literal null", binExpr);
                    }
                    EvaluatedType moreGeneral = rightType.isAssignableTo(leftType) ? leftType : rightType;
                    types.put(expr, moreGeneral.nullable(rightType.isNullable()));
                    break;
                }
                default:
                    throw new TypeCheckFailure("Typechecking of " + binExpr.type + " operator not supported yet");
            }
        } else if (expr instanceof ReservedIdentifierExpression) {
            ReservedIdentifierExpression reservedExpr = (ReservedIdentifierExpression)expr;
            switch (reservedExpr.identifier) {
                case NULL:
                    types.put(expr, EvaluatedType.NULL);
                    break;
                case THIS: {
                    LocalVariable variable = evaluateVariable(null); // null name = this
                    if (variable == null) {
                        error("this not defined in this context", reservedExpr);
                        types.put(expr, EvaluatedType.NULL);
                        break;
                    }
                    types.put(expr, variable.getType());
                    break;
                }
            }
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

    private void error(String message, ASTNode node) {
        problemCollector.error(message, node.row, node.column);
    }

    private void warning(String message, ASTNode node) {
        problemCollector.warning(message, node.row, node.column);
    }

    private MethodCall lookupStaticMethod(String name, EvaluatedType[] argTypes, boolean mustBeExtension) {
        List<ScriptMethod> checkMethods = definedMethods.get(name);
        if (checkMethods != null) {
            searchDefinitionsLoop:
            for (ScriptMethod checkMethod : checkMethods) {
                if (mustBeExtension && !checkMethod.getModifiers().contains(Modifier2.EXTENSION)) {
                    continue;
                }
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
                return new MethodCall(checkMethod);
            }
        }
        {
            Imported<?> imported = imports.get(name);
            if (imported != null && imported.getType() == ImportType.STATIC_METHOD) {
                @SuppressWarnings("unchecked")
                Imported<CtMethod> methodImport = (Imported<CtMethod>)imported;
                try {
                    return new MethodCall(findMethod(
                        methodImport.getOwnedClass().getDeclaredMethods(name),
                        name,
                        false,
                        true,
                        mustBeExtension
                            ? m -> m.hasAnnotation(MethodCall.EXTENSION_METHOD_ANNOTATION)
                            : null,
                        argTypes
                    ), mustBeExtension ? Boolean.TRUE : null);
                } catch (NotFoundException e) {
                    throw new TypeCheckFailure(e);
                }
            }
        }
        return null;
    }

    private static CtMethod findMethod(
        CtMethod[] methods,
        String name,
        boolean checkName,
        boolean staticOnly,
        Predicate<CtMethod> check,
        EvaluatedType... argTypes
    ) {
        if (check == null) {
            check = x -> true;
        }
        try {
            methodCheckLoop:
            for (CtMethod maybe : methods) {
                if (checkName && !maybe.getName().equals(name)) continue;
                if (!isAccessible(maybe)) continue;
                if (staticOnly && !Modifier.isStatic(maybe.getModifiers())) continue;
                if (!check.test(maybe)) continue;
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

    private static CtMethod findMethod(
        CtClass clazz,
        String name,
        boolean checkName,
        boolean staticOnly,
        Predicate<CtMethod> check,
        EvaluatedType... argTypes
    ) {
        return findMethod(clazz.getMethods(), name, checkName, staticOnly, check, argTypes);
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

    static void verifyTypeAssignable(EvaluatedType assignTo, EvaluatedType expr, ProblemCollector problemCollector) {
        if (!expr.isAssignableTo(assignTo)) {
            problemCollector.error(expr + " is not assignable to " + assignTo);
        }
    }

    static void verifyTypeAssignable(String assignTo, CtClass expr, ProblemCollector problemCollector) {
        if (!checkTypeAssignable(assignTo, expr)) {
            problemCollector.error(expr.getName() + " is not assignable to " + assignTo);
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
