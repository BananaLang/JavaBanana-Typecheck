package io.github.bananalang;

import java.io.IOException;

import io.github.bananalang.parse.Parser;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.typecheck.Typechecker;
import javassist.ClassPool;
import javassist.LoaderClassPath;

public class TypeTest {
    public static void main(String[] args) throws IOException {
        StatementList root = new Parser(
            "import java.lang.Class.forName;" +
            "import java.lang.reflect.Method;" +
            "" +
            "def Method meth = forName(\"javassist.ClassPool\").getMethod(\"getDefault\");" +
            "println(meth);"
        ).parse();

        ClassPool cp = new ClassPool(ClassPool.getDefault());
        cp.appendClassPath(new LoaderClassPath(TypeTest.class.getClassLoader()));
        Typechecker typechecker = new Typechecker(cp);
        typechecker.typecheck(root);
    }
}
