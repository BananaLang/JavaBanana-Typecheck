package io.github.bananalang.typecheck;

import java.util.ArrayList;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;

import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.parse.ast.ExpressionNode;
import io.github.bananalang.parse.ast.ExpressionStatement;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.parse.ast.StatementNode;
import io.github.bananalang.parse.ast.StringExpression;
import io.github.bananalang.parse.ast.VariableDeclarationStatement;
import io.github.bananalang.parse.ast.VariableDeclarationStatement.VariableDeclaration;
import javassist.ClassPool;

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
            types.put(root, new MultiEvaluatedType(declTypes));
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
                return new LazyJavassistReferenceType(cp, imported.getQualName());
            }
        }
        throw new IllegalArgumentException("Could not find class " + identifier);
    }

    private EvaluatedType evaluateExpression(ExpressionNode expr) {
        if (expr instanceof StringExpression) {
            types.put(expr, new LazyJavassistReferenceType(cp, "java.lang.String"));
        } else {
            throw new IllegalArgumentException("Typechecking of " + expr.getClass().getSimpleName() + " not supported yet");
        }
        return getType(expr);
    }

    private void verifyTypeAssignable(EvaluatedType assignTo, EvaluatedType expr) {
        if (!expr.isAssignableTo(assignTo)) {
            throw new IllegalArgumentException(expr + " not assignable to " + assignTo);
        }
    }
}
