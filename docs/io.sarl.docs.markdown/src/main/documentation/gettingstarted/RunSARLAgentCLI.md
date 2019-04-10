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


	[:januscmd](janus) [:janusagent](myapp.MyAgent)


The janus command-line tool provides options that will enable you to tune the launching configuration:


	[:januscmd!] --help


One of the command-line options that is usually mandatory is the [:janusjaroption](--jar) option, which enables you to specify the jar files that contains your application:


	[:januscmd!] [:janusjaroption!] path/to/myapp.jar [:janusagent!]


If the [:januscmd:] script indicates to you an error "agent not found", most of the time it is because your application's jar file is not on the class path.
The [:janusjaroption!] option becomes mandatory for specifying the jar file.


## Use the standard java method

### Boot of Janus

The Janus platform provides a [:bootclass:] class. For launching the platform, you must execute this
boot class in a Java Virtual Machine.

The typical command line is:


	java [:cpcli](-cp) [:jarfile](app.jar) [:fullbootclass]{io.janusproject.[:bootclass](Boot)}
	[:Fact:](io.janusproject.Boot)


The option [:cpcli:] specifies the Jar file that contains
the compiled classes. The given [:jarfile:] file is a Jar file that is containing the Janus
platform, the SARL libraries, and the application classes.
The last argument is the fully qualified name of the booting class of Janus: [:fullbootclass:]


###	Specify the Agent to Launch

The example given in the previous section causes an error. Indeed, it is mandatory to
specify the fully qualified name of the agent to launch:


	java -cp app.jar io.janusproject.Boot myapp.MyAgent


<veryimportant>The Janus platform allows to start only one agent from the command line.
If you want to start a collection of agents, you must select one of the following approaches:

* launch a separate Janus platform instance for each agent, or
* launch an agent that is spawning the other agents.
</veryimportant> 


### What is app.jar?

In the previous section, we assume that all the application binary files are
contained into the [:jarfile:] file.

You may replace the [:jarfile:] in the previous command lines by the classpath
that is containing all the jar files required for running your application, including
the Janus jar file(s):


	java -cp /path/to/myapplication.jar:/path/to/[:janusjarfile](io.janusproject.kernel-<version>-with-dependencies.jar) io.janusproject.Boot myapp.MyAgent

The [:janusjarfile:] file may be dowloaded from the [Janus website](http://www.janusproject.io/)

You may also create the [:jarfile:] file with Maven by using the assembly plugin for creating a jar file with all the dependencies inside.


### Janus Command Line Options

The Janus platform provides a collection of command line options.
For obtaining the list of these options, you should type:


	java -cp app.jar io.janusproject.Boot --help


## Use Maven Execution Plugin

Maven provides a plugin for launching an application after automatically building
the application's classpath. This plugin may be used for launching an agent.

### Boot of Janus

Based on the fact that the Janus platform provides a [:bootclass:] class for launching itself,
you may use the Maven execution plugin for classing this booting class.

The typical command line is:


	mvn exec:exec [:mavencliexec](-Dexec.executable)=java [:mavencliargs](-Dexec.args)="-cp %classpath io.janusproject.Boot"


[:Fact:](io.janusproject.Boot)

The option [:mavencliexec:] specifies the Java executable.

The option [:mavencliargs:] contains the command line arguments to pass to Java.
The first argument is the classpath of the project. You must not change `%classpath` because it will be dynamically
replaced by the Maven plugin. 


### Specify the Agent to Launch

The example given in the previous section causes an error.
Indeed, it is mandatory to specify the fully qualified name
of the agent to launch:


	mvn exec:exec -Dexec.executable=java -Dexec.args="-cp %classpath io.janusproject.Boot <qualified_name_of_the_agent>"


<veryimportant>The Janus platform allows to start only one agent from the command line.
If you want to start a collection of agents, you must select
one of the following approaches:

* launch a separate Janus platform instance for each agent, or
* launch an agent that is spawning the other agents.
</veryimportant> 


### Janus Command Line Options

The Janus platform provides a collection of command line options.
For obtaining the list of these options, you should type:


	mvn exec:exec -Dexec.executable=java -Dexec.args="-cp %classpath io.janusproject.Boot --help"



## What's next?

In the next section, we will learn how to launch your SARL project from a Java program.

[Next>](./RunSARLAgentJava.md)


[:Include:](../legal.inc)

