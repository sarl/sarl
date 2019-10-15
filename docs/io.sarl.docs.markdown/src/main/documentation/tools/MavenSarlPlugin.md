# Maven Plugin for the SARL Compiler

[:Outline:]

[:Fact:]{(io.sarl.maven.compiler.Utils).getSarlMavenPluginGroupId == '[:plugingroupid!]'}

[:Fact:]{(io.sarl.maven.compiler.Utils).getSarlMavenPluginArtifactId == '[:pluginname!]'}

[Apache Maven](http://maven.apache.org) is a build automation tool used primarily for Java projects.

Maven addresses two aspects of building software: first, it describes how software is built, 
and second, it describes its dependencies.
Unlike earlier tools like [Apache Ant](https://en.wikipedia.org/wiki/Apache_Ant), it uses conventions
for the build procedure, and only exceptions need to be written down.
An XML file describes the software project being built, its dependencies on other external modules
and components, the build order, directories, and required plug-ins. It comes with pre-defined
targets for performing certain well-defined tasks such as compilation of code and its packaging.

In order to compile SARL code within a Maven-based project, a compilation plug-in is provided, named
[:pluginname:].
 
## Usage

Open the file `pom.xml` of your project, and edit it for obtaining a content similar to the
configuration below.

Replace the version number [:sarl.version:] of SARL with the one you want to use. You could search on the
[Maven Central Repository](http://search.maven.org/) for the last available version.


```xml
 <project>
    ...
    <properties>
       ...
       <sarl.version>[:sarl.version!]</sarl.version>
       <jdk.version>[:user.min.jdk.version!]</jdk.version>
       <project.build.sourceEncoding>[:project.encoding!]</project.build.sourceEncoding>
    </properties>
    ...
    <build>
       <plugins>
          ...
          <plugin>
             <groupId>[:plugingroupid!]</groupId>
             <artifactId>[:pluginname!]</artifactId>
             <version>${sarl.version}</version>
             <extensions>true</extensions>
             <configuration>
                <source>${jdk.version}</source>
                <target>${jdk.version}</target>
                <encoding>${project.build.sourceEncoding}</encoding>
             </configuration>
          </plugin>
		  <plugin>
			<groupId>org.apache.maven.plugins</groupId>
			<artifactId>maven-compiler-plugin</artifactId>
			<version>[:mavencompiler.version!]</version>
			<configuration>
				<source>${jdk.version}</source>
				<target>${jdk.version}</target>
				<encoding>${project.build.sourceEncoding}</encoding>
			</configuration>
		  </plugin>
       </plugins>
    </build>
    ...
    <dependencies>
      ...
       <dependency>
          <groupId>io.sarl.maven</groupId>
          <artifactId>io.sarl.maven.sdk</artifactId>
          <version>${sarl.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```


## Configuration

The configuration section may contains the following elements.

| Property | Maven Goal | Property Type | Description | Default Value |
| -------- | ---------- | ------------- | ----------- | ------------- |
[:Dynamic:]{
	getMavenPluginConfiguration("[:plugingroupid](io.sarl.maven)", "[:pluginname](sarl-maven-plugin)").renderToMarkdown
}



[:Include:](../legal.inc)

