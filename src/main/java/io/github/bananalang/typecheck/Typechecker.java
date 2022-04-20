package io.github.bananalang.typecheck;

import java.lang.reflect.Modifier;
import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Arrays;
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
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.VariableDeclaration;
import io.github.bananalang.typecheck.Imported.ImportType;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;

public final class Typechecker {
    private static final Map<Map.Entry<String, String>, Boolean> cachedAssignableLookups = new HashMap<>();

    private final Map<ASTNode, EvaluatedType> types = new IdentityHashMap<>();
    private final List<Imported<?>> imports = new ArrayList<>();
    private final ClassPool cp;
    private final Map<StatementList, Map<String, EvaluatedType>> scopes = new IdentityHashMap<>();
    private final Deque<StatementList> scopeStack = new ArrayDeque<>();
    private Map<String, EvaluatedType> currentScope = null;

    public Typechecker(ClassPool cp) {
        this.cp = cp;
    }

    public Typechecker() {
        this(ClassPool.getDefault());
    }

    public EvaluatedType typecheck(ASTNode root) {
        evaluateImports(root);
        return typecheck0(root);
    }

    public EvaluatedType getType(ASTNode node) {
        return types.get(node);
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
                    throw new IllegalArgumentException("Duplicate variable " + decl.name);
                }
                EvaluatedType exprType = evaluateExpression(decl.value);
                if (decl.type == null) {
                    declTypes[i] = exprType;
                } else {
                    verifyTypeAssignable(declTypes[i] = evaluateTypeIdentifier(decl.type), exprType);
                }
                currentScope.put(decl.name, declTypes[i]);
            }
        } else {
            throw new IllegalArgumentException("Typechecking of " + root.getClass().getSimpleName() + " not supported yet");
        }
        return getType(root);
    }

    private void evaluateImports(ASTNode root) {
        imports.add(Imported.class_(cp, "java.lang.Class"));
        imports.add(Imported.class_(cp, "java.lang.Object"));
        imports.add(Imported.class_(cp, "java.lang.String"));
        try {
            imports.addAll(Imported.starImport(cp, "banana.builtin.ModuleBuiltin"));
        } catch (IllegalArgumentException e) {
            // No stdlib installed
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
        throw new IllegalArgumentException("Could not find class " + identifier);
    }

    private EvaluatedType evaluateExpression(ExpressionNode expr) {
        if (expr instanceof CallExpression) {
            CallExpression ce = (CallExpression)expr;
            CtClass[] argTypes = new CtClass[ce.args.length];
            for (int i = 0; i < ce.args.length; i++) {
                argTypes[i] = evaluateExpression(ce.args[i]).getJavassist();
            }
            CtMethod method = null;
            if (ce.target instanceof AccessExpression) {
                AccessExpression ae = (AccessExpression)ce.target;
                EvaluatedType targetType = evaluateExpression(ae.target);
                CtClass clazz = targetType.getJavassist();
                method = findMethod(clazz, ae.name, false, argTypes);
            } else if (ce.target instanceof IdentifierExpression) {
                IdentifierExpression ie = (IdentifierExpression)ce.target;
                for (Imported<?> imported : imports) {
                    if (imported.getType() != ImportType.STATIC_METHOD) continue;
                    @SuppressWarnings("unchecked")
                    Imported<CtMethod> methodImport = (Imported<CtMethod>)imported;
                    if (methodImport.getShortName().equals(ie.identifier)) {
                        try {
                            method = findMethod(methodImport.getOwnedClass().getDeclaredMethods(ie.identifier), null, true, argTypes);
                        } catch (NotFoundException e) {
                            throw new IllegalArgumentException(e);
                        }
                        break;
                    }
                }
            }
            if (method == null) {
                throw new IllegalArgumentException("Could not find method associated with " + ce);
            }
            try {
                types.put(expr, new EvaluatedType(method.getReturnType()));
            } catch (NotFoundException e) {
                throw new IllegalArgumentException(e);
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
                throw new IllegalArgumentException("Could not find variable " + ie.identifier);
            }
            types.put(expr, type);
        } else if (expr instanceof StringExpression) {
            types.put(expr, new EvaluatedType(cp, "java.lang.String"));
        } else {
            throw new IllegalArgumentException("Typechecking of " + expr.getClass().getSimpleName() + " not supported yet");
        }
        return getType(expr);
    }

    private static CtMethod findMethod(CtMethod[] methods, String name, boolean staticOnly, CtClass... argTypes) {
        try {
            methodCheckLoop:
            for (CtMethod maybe : methods) {
                if (name != null && !maybe.getName().equals(name)) continue;
                if (!isAccessible(maybe)) continue;
                if (staticOnly && !Modifier.isStatic(maybe.getModifiers())) continue;
                CtClass[] methodParamTypes = maybe.getParameterTypes();
                if (methodParamTypes.length != argTypes.length) continue;
                for (int i = 0; i < methodParamTypes.length; i++) {
                    if (!checkTypeAssignable(methodParamTypes[i].getName(), argTypes[i])) {
                        continue methodCheckLoop;
                    }
                }
                return maybe;
            }
        } catch (NotFoundException e) {
            throw new IllegalArgumentException(e);
        }
        String argStr = Arrays.toString(argTypes);
        throw new IllegalArgumentException("Unable to find method " + name + "(" + argStr.substring(1, argStr.length() - 1) + ")");
    }

    private static CtMethod findMethod(CtClass clazz, String name, boolean staticOnly, CtClass... argTypes) {
        return findMethod(clazz.getMethods(), name, staticOnly, argTypes);
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
        if (assignTo.equals(expr.getName())) {
            return true; // Fast path
        }
        return cachedAssignableLookups.computeIfAbsent(new SimpleImmutableEntry<>(assignTo, expr.getName()), k ->
            forEachSuperclass(expr, clazz -> clazz.getName().equals(assignTo))
        );
    }
}
