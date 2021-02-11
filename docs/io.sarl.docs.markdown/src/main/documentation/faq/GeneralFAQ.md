# General FAQ for SARL

[:Outline:]

## General Questions about SARL

###	What is SARL?

SARL is a statically-typed agent-programming language. SARL aims at providing the fundamental abstractions for
dealing with concurrency, distribution, interaction, decentralization, reactivity, autonomy and dynamic
reconfiguration. These high-level features are now considered as the major requirements for an easy
and practical implementation of modern complex software applications. We are convinced that the
agent-oriented paradigm holds the keys to effectively meet this challenge.

Syntactically and semantically SARL has its roots in the Java programming language but improves on many aspects:

* [Agent specific statements](../index.md#agent-oriented-programming) - provide specific statements for agent programming
* [Type inference](../reference/GeneralSyntax.md) - you rarely need to write down type signatures anymore
* [Lambda expressions](../reference/general/Lambda.md) - concise syntax for anonymous function literals
* [Operator overloading](../reference/general/Operators.md) - make your libraries even more expressive
* [Extension methods](../reference/general/Extension.md) - enhance closed types with new functionality
* [Powerful switch expressions](../reference/general/SwitchExpression.md) - type based switching with implicit casts
* [No statements](../reference/GeneralSyntax.md#details-on-the-sarl-language-elements) - everything is an expression
* Full support for Java generics - including all conformance and conversion rules
* Translates to Java not byte code - understand what is going on and use your code for platforms such as Android or GWT

Unlike other JVM languages, SARL has zero interoperability issues with Java: everything you write interacts with Java exactly as expected. At the same time, SARL is much more concise, readable and expressive.

__The language is platform- and architecture-independent.__

For a brief comparison between SARL, Java and Xtend languages, see the Section
"[Comparison between SARL and other languages](../reference/OOP.md#comparison-between-sarl-and-other-languages)".


### Can I use SARL to make agent-based software?

__Yes__.

SARL may be used to build agent based applications. 
Natively, SARL provides features for agent execution and 
direct communication. The agents may be deployed across
multiple networked computers.


### Can I use SARL to make agent-based simulation software?

__Yes__. 

SARL may be used for agent based simulations. 
Natively, SARL provides features for agent execution and 
direct communication. An extension is provided for
supporting the simulated environments (time management, 
environment model...)


###	Can I use SARL to make holonic software?

__Yes__.

Holon is recursively composed of holons. In SARL, agents are holons. SARL provides a
complete support for holons.


### Can I use SARL to make organizational software?

__Yes__.

An extension to SARL is available that defines an organizational space based on the
[CRIO meta-model](http://www.aspecs.org/CRIO.html) (Capacity-Role-Interaction-Organization).
This meta-model defines a system as a set of organizations  in which roles are defined
and interact together. Agents play roles in organization instances (or groups) and
provides embedded capacity implementations required by the played roles.

Another organizational model may be in another space.


### Is SARL an object-oriented programming language?

__Yes and No__.

While SARL is an agent-oriented programming language,
it is possible to use object-oriented concepts
when writing the agent code (skills...)
A part of the grammar of SARL is inherited from the
[Xbase partial programming language](https://wiki.eclipse.org/Xbase),
provided by the [Xtext framework](https://www.eclipse.org/Xtext/index.html).
It provides statements and rules that correspond to 
object-oriented languages.


### Can I use my Java classes in SARL?

__Yes__.

SARL and Java are 100% interoperable. There are no exceptional cases and you do not have to think 
in two worlds. You can invoke SARL code from Java and vice versa without any surprises or hassles.
[:Fact:]{typeof(Integer)}


### Must I use Maven to create a SARL project?

__No__.

You can create a SARL project with Eclipse without Maven.
Indeed, the SARL Eclipse product supports creation of a SARL project.

However, the SARL developers recommend Maven because it simplifies the
management of your project's dependencies on the SARL libraries.


### What is the SRE?

SRE stands for "SARL Runtime Environment."
The SRE is an implementation of an agent platform which is able to run a SARL program.
The official standard SRE supported by the SARL developers is the
[Janus platform](http://www.janusproject.io).


### What is the difference between SARL and Janus?

SARL is a general-purpose agent-oriented language.
Janus is a runtime environment (SRE) for multi-agent applications
that fully supports the concepts of SARL.

We can compare the SARL universe with the Java universe:


|                          | SARL Universe                 | Java Universe                         |
| ------------------------ | ----------------------------- | ------------------------------------- |
| Language Specification   | SARL Specification            | Java Specification                    |
| Standard Development Kit | SARL SDK                      | J(ava)DK                              |
| Runtime environment      | SRE, e.g. Janus, TinyMAS...   | JVM, e.g. Hotspot, IcedTea, Dalvik... |


### Where can I find information on the release planning of SARL?

The release planning of SARL is detailed on the
[milestones page](https://github.com/sarl/sarl/milestones)
on Github.


### Where can I ask my question?

If your question is not addressed in the FAQ, the reference documents, or
the [existing issues](https://github.com/sarl/sarl/issues), you
may ask the SARL developers on the 
[SARL forum](https://groups.google.com/forum/#!forum/sarl), or 
on the [instant messaging forum](https://gitter.im/sarl/Lobby).


### Where can I find more information and projects related to SARL?

A list of successful projects are given on the [Community](http://sarl.io/community/index.html#sucessstories)
page of the SARL web site.

Additionally, a community driven list of useful SARL libraries, frameworks and software
is maintained on [Github](https://github.com/sarl/awesome-sarl).
This is not a catalog of all the libraries, just a starting point for your explorations.
This list is used by the SARL team to update the official web site of SARL.


## Installation and Execution


### Is my operating system compatible with SARL?

SARL is based on a part of the Eclipse API. Every operating system which has a compatible Java 
Virtual Machine with Eclipse may be used to run SARL. 


### What version of the Java virtual machine is required?

SARL requires the JDK [:sarl-run.min.jdk.version!] or higher to compile and run.
Note that if you plan to create Android applications, you should
configure your JDK to produce 1.6 class files from [:sarl-run.min.jdk.version!] Java code. 
[:Fact:]("[:sarl-run.min.jdk.version!]".shouldBeAtLeastJava)


### Why does SARL display an error on startup?

Most of the time the problem is due to an incompatibility between
the configuration of your operating system or Java virtual machine,
and the SARL Eclipse product.

If a problem occurred, find the ".log" file in which Eclipse is writing
the complete error trace. Usually, it is in your home directory or in
the folder of the SARL Eclipse executable file.


### Why does SARL Eclipse fail on Windows 10?

This is due to a problem in your configuration. Most of the time the log file
(see the previous question) contains the error
"Cannot load 64-bit SWT libraries on 32-bit JVM".

It means that you're trying to run the 64-bit version of the SARL Eclipse with
a Java virtual machine (JVM) that is 32-bit. You should install a fully 64-bit JVM,
or use the 32-bit version of the SARL Eclipse product.

If another error occurs, you should go on the SARL forum and report this problem.


### Why does SARL Eclipse fail on MacOS X?

Several MacOS X users reported that errors when they try to launch the SARL Eclipse product.
Plenty of reasons may be the cause of the failure. As usually, it is always better to
read the ".log" file for determining this cause.

Nevertheless, the three most reported causes of avoidance of the SARL Eclipse launch are:

#### a) The Java virtual machine (JVM) is not valid for running SARL Eclipse

To solve this problem:
* Install the JDK [:sarl-run.min.jdk.version!], and configuring your operating system to use it by default;
* Force the SARL product to use the JDK [:sarl-run.min.jdk.version!] by editing the `eclipse-sarl.ini` file as follows.

[:Fact:]("[:sarl-run.min.jdk.version!]".shouldBeAtLeastJava)

The editing of the `eclipse-sarl.ini` could be done by following the steps:

1. Locate the folder in which your JVM (not the standard Mac JRE) is installed, e.g. `/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/bin`. The previous folder must contains the tools `javac` and `java`.
2. Open the folder in which the SARL `Eclipse.app` was copied
3. Right-click on `Eclipse.app` and select `Show Package Contents`
4. Move to `Contents/Eclipse`
5. Open `eclipse-sarl.ini` with a text editor
6. Add the following lines into the file or update the lines if they exist (there is a line break that must be inside the file content):
```
-vm
/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/bin
```
7. Save and start the SARL IDE.

An complete example of the eclipse-sarl.ini file is:

```
-startup
../Eclipse/plugins/org.eclipse.equinox.launcher_1.5.600.v20191014-2022.jar
--launcher.library
../Eclipse/plugins/org.eclipse.equinox.launcher.cocoa.macosx.x86_64_1.1.1100.v20190907-0426
-vm
/Library/Java/JavaVirtualMachines/jdk1.8.0_202.jdk/Contents/Home/bin
-vmargs
-Xms256m
-Xmx1g
-XstartOnFirstThread
-Dorg.eclipse.swt.internal.carbon.smallFonts
```

#### b) The Gatekeeper of MacOS X blocks the launch of the SARL Eclipse

Because Gatekeeper considers SARL Eclipse as unstable. You could confirm this problem by looking into your `system.log` file and searching for a message that looks like:

```
(application.io.sarl.lang.product.72020573.72020798[78958]): removing service since it exited with consistent failure - OS_REASON_EXEC | Gatekeeper policy blocked execution
```

On order to enable Gatekeper to enable the SARL Eclipse launch, you should type on the Terminal the following command:
```
sudo xattr -rd com.apple.quarantine Eclipse.app
```

Where `Eclipse.app` is the name of the SARL Eclipse application on MacOS X.


#### c) The Eclipse.app application is damaged

This error message is shown up on a dialog box, and you have the choices to move the application to the bin or to cancel the launch.
This error message has the same cause as the Gatekeeper error that is described into the previous section. 


### Why does the SARL product launch but not contain any features related to SARL?

This is due to a problem in your configuration. SARL tools need the Eclipse
framework to be run with a Java Development Kit [:sarl-run.min.jdk.version!] or higher.
You are currently running the SARL product with a lower version of the JDK.

You must run the SARL product with a valid version of the JDK.
Two ways are available for solving this issue:

1. install the JDK [:sarl-run.min.jdk.version!], and configuring your operating system to use it by default; or
2. force the SARL product to use the JDK [:sarl-run.min.jdk.version!] by editing the `eclipse-sarl.ini` file into the folder of the SARL IDE. Add the following parameter on a new line: `-vm path`, where `path` is the path to the binary file `javaw[.exe]` or `java[.exe]` of at least the JDK [:sarl-run.min.jdk.version!].
[:Fact:]("[:sarl-run.min.jdk.version!]".shouldBeAtLeastJava)


### Why does the content assistant not propose any suggestion on MacOS X?

By default in the SARL product, the shortcut key for invoking the content assistant is `Ctrl+Space` or `Apple+Space`.
But, the Spotlight tool on MacOS is also using the same shortcut key.

For enabling the SARL product content assist, we recommend to change its shortcut in the SARL product. Go to:

* Window > Preferences
* General > Keys
* Content Assist

And, change the shortcut key.

![Content assist shortcut change](./ctrlspaceprefs.png)


### Why is the SARL development environment becoming slow or frozen?

Sometimes the SARL product is entering into an invalid state and causes the garbage collector of its Java virtual
machine to be slower and slower until a quasi freeze of the SARL product.
Most of the times, it is due to an error occurring into the code-mining feature
of the SARL editor. This feature shows up (in gray in the editor) the hidden or implicit peaces of code.
Before the code-mining issues are definitively fixed, you could disable the code-mining feature in order
to have your SARL development environment running without problem.

For disabling the code-mining feature, go to:

* Window > Preferences
* SARL > Editors

And, uncheck the option `[:Dynamic:](io.sarl.eclipse.preferences.Messages::SarlEditorPreferencePage_1)`.


## Implementation of SARL Applications


### Is it possible to obtain examples of SARL code for applications?

__Yes__.

The SARL development environment contains a collection of SARL applications that may be used for creating your own applications.
They are called the "SARL examples".

In order to create a fresh project based on of these SARL examples, you have to use the menu `File > New > Example`.
This menu opens a dialog box with a list of examples from which you may select one for creating your fresh project.
The SARL examples have been put into several categories:

* SARL Tutorials: the SARL code that is associated to one of the tutorial from the official documentation pages.
* SARL Examples without user interface: a collection of fully-featured applications without graphical user interface.
* SARL Examples with JavaFX: a collection of fully-featured applications with a JavaFX-based user interface.
* SARL Templates of applications: a collection of templates for creating a fresh SARL application.


### How can we know when an agent has been created fully after being spawn?

An event [:agspawnedevt:] will be emitted when an agent has been created and can
be handled, say by a coordinator, to know the agent is now alive! For example:

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.AgentSpawned
    import io.sarl.core.Logging
    import java.util.Map
    agent X {
        uses Logging
        [:On]
        on [:agspawnedevt](AgentSpawned) {
            info("Agent {0} of type {1} has been created successfully and is now alive!",
                occurrence.agentID, occurrence.agentType)
        }
        [:Off]
    }
[:End:]


### Be careful on the emit of events in "on Initialize"

The `on Initialize` event handler in agents is a bit special, as it is the code ran when an agent is born.
As such, its execution is more "synchronous" than other on-behavior rules. In particular:

1. Any event emitted within an `on Initialize`, will not be processed until that
   `on Initialize` code finishes. So, your agent initialization should not depend
   (and wait) on any fired event being processed, as they won't!
2. When spawning an agent in `on Initialize`, the spawn instructions will return only
   after the agent has been created. However, creation of the agent (i.e., of the
   corresponding object) does not include initialization of the agent via its 
   `on Initialize` handler. Said so, the Java thread manager may process those
   initialization processes of the new agent before continuing with the execution
   of the spawning agent (and this seems to be the case in many Linux boxes
   where the executor service of Java tends to have the same behavior during
   all the runs). If you change computer, it may be different. In the following
   example, the thread executor service of Java seems to give the priority to
   the `on Initialize` of [:agenttwoname:] instead of continuing the run of the
   spawn function.

[:Success:]
    package io.sarl.docs.faq.general
    import io.sarl.core.Initialize
    import io.sarl.core.Logging
    import io.sarl.core.Lifecycle
    [:On]
    agent Agent1 {
        uses Logging, Lifecycle
        var agent_name = "agent1"
        on Initialize {
            info(agent_name + " spawned")
            info(agent_name + " spawning Agent2")
            spawn(Agent2)
            info(agent_name + " end")
        }
    }

    agent [:agenttwoname](Agent2) {
        uses Logging
        var agent_name = "agent2"
        on Initialize {
            info(agent_name + " spawned")
            info(agent_name + " sleeping")
            Thread::sleep(5000)
            info(agent_name + " woke up")
            info(agent_name + " end")
        }
        on Initialize {
            info(agent_name + " init2")
            info(agent_name + " init2 end")
        }
    }
[:End:]


The output has been:

```text
Launching the agent: Agent1
agent1 spawned
agent1 spawning Agent2
agent2 spawned
agent2 init2
agent2 sleeping
agent2 init2 end
agent2 woke up
agent2 end
agent1 end
```

Here it appears as the `on Initialize` behaviors have been run all before
the execution resumes after the `spawn()` statement, but this is just one way
and one should not rely on that behavior being guaranteed: once the spawned
agent is created, the `spawn()` commands returns.



### What is the list of all the error, warning and information messages that may be generated by the SARL compiler?

We provide a [page](../reference/CompilerErrors.md) that lists all the issue messages from the SARL compiler. 

Additionally, several run-time errors from the [Janus framework](../tools/Janus.md) are explained [here](../tools/Janus.md#3-list-of-errors-and-warnings). 



### How can the warnings given by the SARL compiler be avoided?

You can use `@SupressWarnings(...)` annotations in the entities you do not want
to be warned. For example, a typical warning SARL will give is lack of
synchronization for variables that can be accessed/edited concurrently:
```text
[WARNING] The field noToSpawn should be synchronized for avoiding value inconsistency
due to parallel execution. [BootMultiSWIAgents.sarl:70]
```

To get rid of such warnings, assuming you are aware of the potential issue and
have planned for it, you can do:

[:Success:]
    package io.sarl.docs.faq.general
    [:On]
    @SuppressWarnings("potential_field_synchronization_problem")
    agent BootMultiSWIAgents {
        //...
    }
[:End:]


See the [Issue Codes](https://github.com/sarl/sarl/blob/master/main/coreplugins/io.sarl.lang/src/io/sarl/lang/validation/IssueCodes.java)
for a complete list of what can be suppressed.



## Contribute to SARL

### Where are the sources for SARL?

The sources for SARL are available on
[Github](https://github.com/sarl/sarl).
Details for getting the source code may be found on the
[download page]([:sarlUrl!]/download/). 


### How can I find the current issues?

SARL Core Developers use [Github](https://github.com/sarl/sarl)
to manage bug tracking and project workflow. 
The issues are listed on [Github](https://github.com/sarl/sarl/issues). 


### How can I report a problem or a bug in SARL components?

You should submit your issue on [this page](https://github.com/sarl/sarl/issues/new).


[:Include:](../legal.inc)
