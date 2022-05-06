package io.github.bananalang;

import java.io.IOException;

import io.github.bananalang.compilecommon.problems.GenericCompilationFailureException;
import io.github.bananalang.compilecommon.problems.ProblemCollector;
import io.github.bananalang.parse.Parser;
import io.github.bananalang.parse.ast.StatementList;
import io.github.bananalang.typecheck.Typechecker;
import javassist.ClassPool;
import javassist.LoaderClassPath;

public class TypeTest {
    public static void main(String[] args) throws IOException {
        ProblemCollector problemCollector = new ProblemCollector();

        try {
            StatementList root = new Parser(
                "def lazy var myVar = myExpensiveFunc();\n" +
                "println(\"created variable\");\n" +
                "def String myExpensiveFunc() {\n" +
                    "def global var myThing = \"\";\n" +
                    "println(\"initialized\");\n" +
                    "return \"hello\";\n" +
                "}\n" +
                "println(myVar);\n",
                problemCollector
            ).parse();

            ClassPool cp = new ClassPool(ClassPool.getDefault());
            cp.appendClassPath(new LoaderClassPath(TypeTest.class.getClassLoader()));
            Typechecker typechecker = new Typechecker(cp, problemCollector);
            typechecker.typecheck(root);
        } catch (GenericCompilationFailureException e) {
        }
        System.out.println();
        System.out.println(problemCollector.ansiFormattedString());
    }
}
