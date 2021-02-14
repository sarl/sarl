# Run SARL Agent from the Command Line

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
This document explains how to launch an agent on the
[Janus platform](http://www.janusproject.io) from the command line.

Three methods could be used for launching an agent with Janus:

* [Using the provided janus command-line tool](#use-the-janus-command-line-tool);
* [Using the standard java method](#use-the-standard-java-method);
* [Using Maven execution plugin](#use-maven-execution-plugin).



## Use the Janus command-line tool

The SARL project provides a [command-line tool for launching agents](../tools/Janus.md) on the Janus runtime environment.


### Download the janus command-line tool

You could download this command line tool, named "janus" on the [downloading page of SARL]([:sarlUrl!]/download/index.html).

### Launching the agent

For launching an agent, you must launch the command-line tool with the fully-qualified
name of the agent as parameter, [:janusagent:] in the following example.


```text
[:januscmd](janus) [:janusagent](myapp.MyAgent)
```


The janus command-line tool provides options that will enable you to tune the launching configuration:

```text
[:januscmd!] --help
```


One of the command-line options that is usually mandatory is the [:janusjaroption](--jar) option, which enables you to specify the jar files that contains your application:

```text
[:januscmd!] [:janusjaroption!] path/to/myapp.jar [:janusagent!]
```


If the [:januscmd:] script indicates to you an error "agent not found", most of the time it is because your application's jar file is not on the class path.
The [:janusjaroption!] option becomes mandatory for specifying the jar file.


## Use the standard java method

### Boot of Janus

The Janus platform provides a [:bootclass:] class. For launching the platform, you must execute this
boot class in a Java Virtual Machine.

The typical command line is:

```text
java [:cpcli](-cp) [:jarfile](app.jar) [:fullbootclass]{io.sarl.sre.boot.[:bootclass](Boot)}
```
[:Fact:](io.sarl.sre.boot.Boot)


The option [:cpcli:] specifies the Jar file that contains
the compiled classes. The given [:jarfile:] file is a Jar file that is containing the Janus
platform, the SARL libraries, and the application classes.
The last argument is the fully qualified name of the booting class of Janus: [:fullbootclass:]


###  Specify the Agent to Launch

The example given in the previous section causes an error. Indeed, it is mandatory to
specify the fully qualified name of the agent to launch:

```text
java -cp app.jar [:fullbootclass!] myapp.MyAgent
```


> **_Very Important Note:_** The Janus platform allows to start only one agent from the command line.
> If you want to start a collection of agents, you must select one of the following approaches:
> 
> * launch a separate Janus platform instance for each agent, or
> * launch an agent that is spawning the other agents.


### What is app.jar?

In the previous section, we assume that all the application binary files are
contained into the [:jarfile:] file.

### app.jar by hand

You may replace the [:jarfile:] in the previous command lines by the classpath
that is containing all the jar files required for running your application, including
the Janus jar file(s):

```text
java -cp /path/to/myapplication.jar:/path/to/[:janusjarfile](io.janusproject.kernel-<version>-with-dependencies.jar) [:fullbootclass!] myapp.MyAgent
```

The [:janusjarfile:] file may be dowloaded from the [Janus website](http://www.janusproject.io/)

### Creating app.jar with maven-assembly-plugin

You may also create the [:jarfile:] file with Maven by using the assembly plugin for creating a jar file with all the dependencies inside.
To do so, you have to update the `pom.xml` file of your project and to define the assembly specification.

The content of the `pom.xml` must include the assembly plugin definition:

```xml
<plugin>
  <groupId>org.apache.maven.plugins</groupId>
  <artifactId>maven-assembly-plugin</artifactId>
  <version>3.3.0</version>
  <executions>
    <execution>
      <id>make-assembly-with-deps</id>
      <phase>package</phase>
      <goals>
        <goal>single</goal>
      </goals>
      <configuration>
        <descriptors>
          <descriptor>with-dependencies.xml</descriptor>
        </descriptors>
      </configuration>
    </execution>
  </executions>
</plugin>
```

The previous definition mentions the file `with-dependencies.xml` that contains the assembly specification.
The content of this file could be:

```xml
<assembly xmlns="http://maven.apache.org/plugins/maven-assembly-plugin/assembly/1.1.0"
 xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance"
 xsi:schemaLocation="http://maven.apache.org/plugins/maven-assembly-plugin/assembly/1.1.0 http://maven.apache.org/xsd/assembly-1.1.0.xsd">
  <id>with-dependencies</id>
  <formats>
    <format>jar</format>
  </formats>
  <includeBaseDirectory>false</includeBaseDirectory>
  <dependencySets>
    <dependencySet>
      <unpack>true</unpack>
      <scope>runtime</scope>
    </dependencySet>
  </dependencySets>
  <containerDescriptorHandlers>
    <!-- Merge service description's files in a proper way -->
    <containerDescriptorHandler>
      <handlerName>metaInf-services</handlerName>
    </containerDescriptorHandler>
  </containerDescriptorHandlers>
</assembly>
```

The tag `containerDescriptorHandlers` is **very important** to be present into the definition.
Without this tag, the SARL and Janus services will not be correctly merged into the
generated Jar file with all the dependencies.

> **_Caution:_** You must use the version 3.3.0 (or higher) of `maven-assembly-plugin` to have access
> to the mentioned capability.


### Janus Command Line Options

The Janus platform provides a collection of command line options.
For obtaining the list of these options, you should type:

```text
java -cp app.jar [:fullbootclass!] --help
```


## Use Maven Execution Plugin

Maven provides a plugin for launching an application after automatically building
the application's classpath. This plugin may be used for launching an agent.

### Boot of Janus

Based on the fact that the Janus platform provides a [:bootclass:] class for launching itself,
you may use the Maven execution plugin for classing this booting class.

The typical command line is:

```text
mvn exec:exec [:mavencliexec](-Dexec.executable)=java [:mavencliargs](-Dexec.args)="-cp %classpath [:fullbootclass!]"
```

[:Fact:](io.sarl.sre.boot.Boot)

The option [:mavencliexec:] specifies the Java executable.

The option [:mavencliargs:] contains the command line arguments to pass to Java.
The first argument is the classpath of the project. You must not change `%classpath` because it will be dynamically
replaced by the Maven plugin. 


### Specify the Agent to Launch

The example given in the previous section causes an error.
Indeed, it is mandatory to specify the fully qualified name
of the agent to launch:

```text
mvn exec:exec -Dexec.executable=java -Dexec.args="-cp %classpath [:fullbootclass!] <qualified_name_of_the_agent>"
```


> **_Very Important Note:_** The Janus platform allows to start only one agent from the command line.
> If you want to start a collection of agents, you must select
> one of the following approaches:
>
> * launch a separate Janus platform instance for each agent, or
> * launch an agent that is spawning the other agents.


### Janus Command Line Options

The Janus platform provides a collection of command line options.
For obtaining the list of these options, you should type:

```text
mvn exec:exec -Dexec.executable=java -Dexec.args="-cp %classpath [:fullbootclass!] --help"
```



## What's next?

In the next section, we will learn how to launch your SARL project from a Java program or a SARL class.

[Next>](./RunSARLAgentJava.md)


[:Include:](../legal.inc)
