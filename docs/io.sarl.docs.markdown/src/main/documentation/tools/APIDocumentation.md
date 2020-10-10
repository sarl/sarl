# API Documentation for SARL

[:Outline:]

Good API documentation is one of the many factors contributing to the overall success of a software project.
Fortunately, SARL provide a tool for generating API documentation from comments present in the source code.

Because the SARL compiler generates Java code, the SARL documentation tool is based 
on the famous [Javadoc tool](https://docs.oracle.com/javase/9/javadoc/javadoc-command.htm).
Javadoc is a documentation generator created by Sun Microsystems (now Oracle) for the
Java language for generating API documentation in HTML format from Java
source code. The HTML format is used for adding the convenience of being able to hyperlink related documents together.

The ["doc comments" format](https://www.oracle.com/technetwork/articles/java/index-137868.html) used by
Javadoc is the de facto industry standard for documenting Java classes. SARL uses the 
same standard format.

Documentation does not affect performance in neither in SARL not Java as all comments are removed at compilation time. Writing
comments and documentation is for better understanding the code and thus better maintaining it.

## Doclet for SARL

Javadoc also provides an API for creating doclets and taglets, which allows users to analyze the
structure of an application. This enables to generate specific documentation pages.

SARL comes with a specific doclet that replaces all the elements written with the Java syntax 
by their equivalent into the SARL syntax.
The SARL doclet is provided into the Maven module:

* Group Id: `io.sarl.docs`
* Artifact Id: `io.sarl.docs.doclet`

See the next sections for details on the usage of the SARL doclet.


## Maven Generation

The Maven Javadoc Plugin uses the Javadoc tool to generate documentation for the specified project.
The Maven Javadoc Plugin gets the parameter values that will be used from the plugin configuration
specified in the POM file.

To generate output from an alternate doclet, add configuration similar to the following to your POM 
file.

```xml
<build>
    <plugins>
        <plugin>
            <groupId>org.apache.maven.plugins</groupId>
            <artifactId>maven-javadoc-plugin</artifactId>

            <configuration>
                <doclet>io.sarl.docs.doclet.SarlDoclet</doclet>
                <docletArtifact>
                    <groupId>io.sarl.docs</groupId>
                    <artifactId>io.sarl.docs.doclet</artifactId>
                    <version>${sarl.version}</version>
                </docletArtifact>
            </configuration>
        </plugin>
    </plugins>
</build>
```

## Command-Line Generation

As described into the [Javadoc documentation](https://docs.oracle.com/javase/9/javadoc/javadoc-command.htm),
the javadoc command has options for doclets. 
The Javadoc command provides [:docletoption:] option for specifying a custom doclet.

A typical command-line to launch is:

```text
javadoc [:docletoption](-doclet) io.sarl.docs.doclet.SarlDoclet
        [:cpoption](-cp) [:docletjarfile](doclet-[:sarl.specification.release_version!].jar):path_to_sources
        -source 1.8
        -sourcepath path_to_sources
        -d path_to_documentation
```

In order to use the SARL doclet, you have to specific the name of the doclet with the [:docletoption:].
You must also include the SARL doclet binary file into the class path with the [:cpoption:].
In the example above, the name of the SARL doclet's jar file is [:docletjarfile:].


## Specific Options of the SARL Doclet

The SARL doclet provides specific options that may be used into the documentation text 
itself, or from the command-line.

### Doclet Options

The SARL doclet has the same options as the [Standard doclet](https://docs.oracle.com/javase/9/javadoc/javadoc-command.htm#JSJAV-GUID-F9E5D57D-5A94-4043-A010-B24511A7BAB2).

### Exclude elements from the documentation

It is possible to exclude elements from the documentation by adding the [:excludetag:] 
into the SARL documentation. For example:

[:Success:]
[:On]
/** This is an example of excluded documentation.
 *
 * [:excludetag](@ExcludeFromApidoc)
 */
class TheClass {
}
[:End:]



[:Include:](../legal.inc)
