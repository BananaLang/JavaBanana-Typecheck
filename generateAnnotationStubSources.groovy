import java.nio.file.Files
import java.nio.file.Paths
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit
import javassist.ClassPool
import javassist.CtClass
import javassist.NotFoundException
import javassist.bytecode.Descriptor
import javassist.bytecode.SignatureAttribute

class FieldOrMethodInfo {
    String ijSignature
    String annotation
    String[] argAnnotations
}

def threadCount = Runtime.runtime.availableProcessors()
log.info "Generating annotation stubs with $threadCount threads..."
def start = System.currentTimeMillis()

def jdkAnnotationsDir = Paths.get("${project.basedir}/jdkAnnotations/jdkAnnotations")
def stubsBaseDir = new File("${project.build.directory}/generated-sources/banana/stubs")
stubsBaseDir.mkdirs()

Executors.newFixedThreadPool(threadCount).with {
    Files.walk(jdkAnnotationsDir).filter { !Files.isDirectory(it) }.forEach { annotationPackage ->
        execute {
            log.info "Processing $annotationPackage"

            def packageName = jdkAnnotationsDir.relativize(annotationPackage).toString().replaceAll(File.separator, '.')
            packageName = packageName.substring(0, packageName.length() - 16)
            if (packageName.startsWith('com.sun.jdi')) return

            def classMap = new HashMap()
            def root = new XmlSlurper().parse(annotationPackage.toFile())
            for (def annoObj : root.item) {
                def annoName = annoObj.@name.text()
                if (annoName.indexOf(' ') == -1) {
                    // Package
                    continue
                }
                def className = annoName.substring(0, annoName.indexOf(' '))
                if (classMap[className] == null) {
                    classMap[className] = new HashMap()
                }
                def annotation = null
                for (def definedAnno : annoObj.annotation) {
                    def definedAnnoName = definedAnno.@name.text()
                    if (definedAnnoName == 'org.jetbrains.annotations.Nullable') {
                        annotation = 'Nullable'
                        break
                    } else if (definedAnnoName == 'org.jetbrains.annotations.NotNull') {
                        annotation = 'NonNull'
                        break
                    }
                }
                try {
                    def argN = Integer.parseInt(annoName.substring(annoName.lastIndexOf(' ') + 1))
                    // This is a method argument
                    if (annotation == null) {
                        continue // Next argument
                    }
                    def annoSig = annoName.substring(annoName.indexOf(' ') + 1, annoName.lastIndexOf(' '))
                    if (classMap[className][annoSig] == null) {
                        def argString = annoSig.substring(annoSig.indexOf('('), annoSig.length() - 1)
                        classMap[className][annoSig] = new FieldOrMethodInfo(
                            ijSignature: annoSig,
                            annotation: null,
                            argAnnotations: new String[argString.count(',') + 1]
                        )
                    }
                    classMap[className][annoSig].argAnnotations[
                        Integer.parseInt(annoName.substring(annoName.lastIndexOf(' ') + 1))
                    ] = annotation
                } catch (NumberFormatException e) {
                    // This is a method itself
                    def annoSig = annoName.substring(annoName.indexOf(' ') + 1)
                    if (annoSig.indexOf('(') == -1) {
                        // Field
                        classMap[className][annoSig] = new FieldOrMethodInfo(
                            ijSignature: annoSig,
                            annotation: annotation,
                            argAnnotations: null
                        )
                    } else {
                        // Method
                        def argString = annoSig.substring(annoSig.indexOf('('), annoSig.length() - 1)
                        classMap[className][annoSig] = new FieldOrMethodInfo(
                            ijSignature: annoSig,
                            annotation: annotation,
                            argAnnotations: argString.empty ? new String[0] : new String[argString.count(',') + 1]
                        )
                    }
                }
            }

            def outputDir = new File(stubsBaseDir, packageName.replace('.' as char, '/' as char))
            outputDir.mkdirs()
            for (def classEntry : classMap.entrySet()) {
                def classBaseName = classEntry.key.substring(classEntry.key.lastIndexOf('.') + 1)
                if (packageName == 'java.lang' && classBaseName == 'Object') continue
                if (packageName == 'java.util' && classBaseName == 'Entry') continue
                try (def output = new PrintWriter(new File(outputDir, classBaseName + '.java'))) {
                    def javassist
                    try {
                        javassist = ClassPool.default.get("${packageName}.${classBaseName}")
                    } catch (NotFoundException e) {
                        log.warn "Could not read class file for ${packageName}.${classBaseName}, type parameters will be missing", e
                        javassist = null
                    }
                    output.println "package banana.stubs.$packageName;"
                    output.println ''
                    output.println 'import banana.internal.annotation.Nullable;'
                    output.println 'import banana.internal.annotation.NonNull;'
                    output.println ''
                    output.println '/**'
                    output.println " * Generated from IntelliJ jdkAnnotations.jar"
                    if (javassist == null) {
                        output.println ' * WARNING: Could not read class file, type parameters will be missing'
                    }
                    output.println ' */'
                    output.println '@java.lang.SuppressWarnings("all")'
                    output.print "class $classBaseName"
                    if (javassist != null && javassist.genericSignature != null) {
                        def classSig = SignatureAttribute.toClassSignature(javassist.genericSignature)
                        if (classSig.parameters.length > 0) {
                            output.print '<'
                            for (def i = 0; i < classSig.parameters.length; i++) {
                                if (i > 0) {
                                    output.print ', '
                                }
                                output.print classSig.parameters[i]
                            }
                            output.print '>'
                        }
                    }
                    output.println ' {'
                    for (def annoData : classEntry.value.values()) {
                        output.print '    '
                        if (annoData.annotation != null) {
                            output.print "@${annoData.annotation} "
                        }
                        if (annoData.argAnnotations == null) {
                            // Field
                            output.println "public Object ${annoData.ijSignature} = null;"
                        } else {
                            // Method
                            def lparenIndex = annoData.ijSignature.indexOf('(')
                            def header = annoData.ijSignature.substring(0, lparenIndex)
                            def isConstructor = header == classBaseName
                            output.print 'public '
                            if (isConstructor) {
                                output.print classBaseName
                            } else {
                                if (javassist != null) {
                                    try {
                                        def javassistMethods = javassist.getDeclaredMethods(
                                            annoData.ijSignature.substring(
                                                annoData.ijSignature.lastIndexOf(' ', lparenIndex) + 1, lparenIndex
                                            )
                                        )
                                        def paramCount = annoData.ijSignature[lparenIndex + 1] == ')' ? 0 : annoData.ijSignature.count(',') + 1
                                        for (def javassistMethod : javassistMethods) {
                                            if (javassistMethods.length != 1) {
                                                if (Descriptor.numOfParameters(javassistMethod.signature) != paramCount) {
                                                    continue
                                                }
                                                def jParamTypes = javassistMethod.parameterTypes
                                                def rParamTypes = annoData.ijSignature.substring(
                                                    lparenIndex + 1, annoData.ijSignature.length() - 1
                                                ).split(', ')
                                                def allEqual = true
                                                for (def i = 0; i < paramCount; i++) {
                                                    def rParamGenericIndex = rParamTypes[i].indexOf('<')
                                                    def jParamType = jParamTypes[i].name
                                                    def rParamType = rParamGenericIndex == -1 ? rParamTypes[i] : rParamTypes[i].substring(0, rParamGenericIndex)
                                                    if (rParamType.endsWith('...')) {
                                                        rParamType = rParamType.substring(0, rParamType.length() - 3) + '[]'
                                                    }
                                                    def rParamArrayCount = rParamType.count('[]')
                                                    if (rParamArrayCount != jParamType.count('[]')) {
                                                        allEqual = false
                                                        break
                                                    }
                                                    if (rParamArrayCount != 0) {
                                                        jParamType = jParamType.substring(0, jParamType.indexOf('[]'))
                                                        rParamType = rParamType.substring(0, rParamType.indexOf('[]'))
                                                    }
                                                    if (
                                                        jParamType != rParamType &&
                                                        !(rParamType.length() == 1 && Character.isUpperCase(rParamType.charAt(0))) // It's ugly, but it works
                                                    ) {
                                                        allEqual = false
                                                        break
                                                    }
                                                }
                                                if (!allEqual) {
                                                    continue
                                                }
                                            }
                                            if (javassistMethod.genericSignature != null) {
                                                def methodSig = SignatureAttribute.toMethodSignature(javassistMethod.genericSignature)
                                                if (methodSig.typeParameters.length > 0) {
                                                    output.print '<'
                                                    for (def i = 0; i < methodSig.typeParameters.length; i++) {
                                                        if (i > 0) {
                                                            output.print ', '
                                                        }
                                                        output.print methodSig.typeParameters[i]
                                                    }
                                                    output.print '> '
                                                }
                                                break
                                            }
                                        }
                                    } catch (NotFoundException e) {
                                        log.warn 'Could not find method for type parameters', e
                                        output.print "/* WARNING: couldn't find method for type parameters */ "
                                    }
                                }
                                output.print header
                            }
                            output.print '('
                            if (annoData.ijSignature[lparenIndex + 1] != ')') {
                                def erasedSignature = annoData.ijSignature.substring(0, lparenIndex)
                                for (def i = lparenIndex, depth = 0; i < annoData.ijSignature.length(); i++) {
                                    if (annoData.ijSignature[i] == '<') {
                                        depth++
                                    } else if (annoData.ijSignature[i] == '>') {
                                        depth--
                                    } else if (depth == 0) {
                                        erasedSignature += annoData.ijSignature[i]
                                    }
                                }
                                for (def i = 0, j = lparenIndex + 1;;) {
                                    def nextJ = erasedSignature.indexOf(',', j)
                                    def argType = erasedSignature.substring(
                                        j, nextJ == -1 ? erasedSignature.length() - 1 : nextJ
                                    )
                                    if (annoData.argAnnotations[i] != null) {
                                        output.print "@${annoData.argAnnotations[i]} "
                                    }
                                    output.print "$argType arg${i++}"
                                    if (nextJ == -1) break
                                    output.print ', '
                                    j = nextJ + 2
                                }
                            }
                            if (isConstructor) {
                                output.println ') {}'
                            } else {
                                output.print ') { return '
                                switch (annoData.ijSignature.substring(0, annoData.ijSignature.indexOf(' '))) {
                                    case 'void':
                                        break
                                    case 'byte':
                                    case 'short':
                                    case 'char':
                                    case 'int':
                                    case 'float':
                                    case 'long':
                                    case 'double':
                                        output.print '0'
                                        break
                                    case 'boolean':
                                        output.print 'false'
                                        break
                                    default:
                                        output.print 'null'
                                        break
                                }
                                output.println '; }'
                            }
                        }
                    }
                    output.println('}')
                }
            }
        }
    }
    shutdown()
    while (!awaitTermination(10, TimeUnit.SECONDS)) {
        log.info "Still processing ${taskCount - completedTaskCount} package(s)"
    }
}

def end = System.currentTimeMillis()
log.info "Finished generating annotation stubs in ${(end - start) / 1000.0}s"
