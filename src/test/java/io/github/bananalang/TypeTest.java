package io.github.bananalang;

import java.io.IOException;

import io.github.bananalang.parse.Parser;
import io.github.bananalang.parse.ast.ASTNode;
import io.github.bananalang.typecheck.Typechecker;

public class TypeTest {
    public static void main(String[] args) throws IOException {
        ASTNode root = new Parser("def var str = \"hello\";").parse();
        Typechecker typechecker = new Typechecker();
        typechecker.typecheck(root);
    }
}
