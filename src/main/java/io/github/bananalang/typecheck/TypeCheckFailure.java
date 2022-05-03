package io.github.bananalang.typecheck;

import io.github.bananalang.parse.ast.ASTNode;

public class TypeCheckFailure extends IllegalArgumentException {
    public final int row, column;

    public TypeCheckFailure(String reason) {
        super(reason);
        row = 0;
        column = 0;
    }

    public TypeCheckFailure(String reason, ASTNode node) {
        super(reason);
        row = node.row;
        column = node.column;
    }

    public TypeCheckFailure(Throwable cause) {
        super(cause);
        row = 0;
        column = 0;
    }
}
