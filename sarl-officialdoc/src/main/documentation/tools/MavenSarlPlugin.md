# Maven Plugin for the SARL Compiler - sarl-maven-plugin

[:Outline:]

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
 
## Note on the application classpath

The [:pluginname!] plugin does not deal with the run-time classpath of the application.
It is a tool that is compiling the SARL code to the target language, e.g. Java.
It means that it does not enforce if a SARL runtime environment is installed and used in your application.

For launching a SARL application, please refer to one of:

* [Running an agent from the command-line shell](../getstarted/RunSARLAgentCLI.md)
* [Running an agent inside SARL Eclipse environment](../getstarted/RunSARLAgentEclipse.md)
* [Running an agent from a Java program progammatically](../getstarted/RunSARLAgentJava.md)


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
       <jdk.version>[:sarl-user.min.jdk.version!]</jdk.version>
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
                <encoding>${project.build.sourceEncoding}</encoding>
             </configuration>
          </plugin>
		  <plugin>
			<groupId>org.apache.maven.plugins</groupId>
			<artifactId>maven-compiler-plugin</artifactId>
			<version>[:maven.compiler.version!]</version>
			<configuration>
				<source>${jdk.version}</source>
				<encoding>${project.build.sourceEncoding}</encoding>
			</configuration>
		  </plugin>
       </plugins>
    </build>
    ...
    <dependencies>
      ...
       <dependency>
          <groupId>[:sarlsdkgroupid](io.sarl.sdk)</groupId>
          <artifactId>[:sarlsdkartifactid](sdk)</artifactId>
          <version>${sarl.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlMavenPluginGroupId(null) == '[:plugingroupid!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlMavenPluginArtifactId(null) == '[:pluginname!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlSdkGroupId(null) == '[:sarlsdkgroupid!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlSdkArtifactId(null) == '[:sarlsdkartifactid!]'}

## Configuration

The configuration section may contains the following elements.

| Property | Maven Goal | Property Type | Description | Default Value |
| -------- | ---------- | ------------- | ----------- | ------------- |
[:Dynamic:]{
	getMavenPluginConfiguration("[:plugingroupid](io.sarl.lang)", "[:pluginname](sarl-maven-plugin)").renderToMarkdown
}



[:Include:](../includes/legal.inc)

