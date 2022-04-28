Using as a dependency:

```xml
<repositories>
  <repository>
    <id>bananalang</id>
    <url>https://bananalang.github.io/maven</url>
  </repository>
</repositories>

<dependency>
  <groupId>io.github.bananalang</groupId>
  <artifactId>JavaBanana-Typecheck</artifactId>
  <version>(one of the versions below)</version>
</dependency>
```

![maven release](https://img.shields.io/badge/dynamic/xml.svg?label=maven%20release&color=blue&query=%2Fmetadata%2Fversioning%2Frelease&url=https%3A%2F%2Fbananalang.github.io%2Fmaven%2Fio%2Fgithub%2Fbananalang%2FJavaBanana-Typecheck%2Fmaven-metadata.xml)
![maven latest](https://img.shields.io/badge/dynamic/xml.svg?label=maven%20latest&color=blue&query=%2Fmetadata%2Fversioning%2Fversions%2Fversion%5Blast%28%29%5D&url=https%3A%2F%2Fbananalang.github.io%2Fmaven%2Fio%2Fgithub%2Fbananalang%2FJavaBanana-Typecheck%2Fmaven-metadata.xml)

## Building

This repo, unlike other JavaBanana repos, requires you to do a clone with `--recurse-submodules`, and requires you to use Java 9 or later to build it. Both requirements are do to the annotation stub generation process.
