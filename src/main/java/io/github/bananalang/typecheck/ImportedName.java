package io.github.bananalang.typecheck;

public class ImportedName {
    private final String qualName, className;

    private ImportedName(String className) {
        this.qualName = className;
        this.className = className.substring(className.lastIndexOf('.') + 1);
    }

    public static ImportedName className(String className) {
        return new ImportedName(className);
    }

    public String getQualName() {
        return qualName;
    }

    public String getClassName() {
        return className;
    }
}
