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

* [Agent specific statements](../index.md#5-2-agent-oriented-programming) - provide specific statements for agent programming
* [Type inference](../reference/GeneralSyntax.md) - you rarely need to write down type signatures anymore
* [Lambda expressions](../reference/general/Lambda.md) - concise syntax for anonymous function literals
* [Operator overloading](../reference/general/Operators.md) - make your libraries even more expressive
* [Extension methods](../reference/general/Extension.md) - enhance closed types with new functionality
* [Powerful switch expressions](../reference/general/SwitchExpression.md) - type based switching with implicit casts
* [No statements](../reference/GeneralSyntax.md#4-details-on-the-sarl-language-elements) - everything is an expression
* Full support for Java generics - including all conformance and conversion rules
* Translates to Java not bytecode - understand what is going on and use your code for platforms such as Android or GWT

Unlike other JVM languages, SARL has zero interoperability issues with Java: everything you write interacts with Java exactly as expected. At the same time, SARL is much more concise, readable and expressive.

__The language is platform- and architecture-independent.__

For a brief comparison between SARL, Java and Xtend languages, see the Section
"[Comparison between SARL and other languages](../reference/OOP.md#1-comparison-between-sarl-and-other-languages)".


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
[CRIO meta-model](http://www.aspecs.org/CRIO) (Capacity-Role-Interaction-Organization).
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


|                          | SARL Universe       | Java Universe               |
| ------------------------ | ------------------- | --------------------------- |
| Language Specification   | SARL Specification  | Java Specification          |
| Standard Development Kit | SARL SDK            | J(ava)DK                    |
| Runtime environment      | Janus, TinyMAS...   | Hotspot, IcedTea, Dalvik... |


### Where can I find information on the release planning of SARL?

The release planning of SARL is detailed on the
[milestones page](https://github.com/sarl/sarl/milestones)
on Github.


### Where can I ask my question?

If your question is not addressed in the FAQ, the reference documents, or
the [existing issues](https://github.com/sarl/sarl/issues), you
may ask the SARL developers on 
[the SARL forum](https://groups.google.com/forum/#!forum/sarl).


### Where can I find more information and projects related to SARL?

A community driven list of useful SARL libraries, frameworks and software
is maintained on [Github](https://github.com/sarl/awesome-sarl).
This is not a catalog of all the libraries, just a starting point for your explorations.
This list is used by the SARL team to update the official web site of SARL.


## Installation and Execution


### Is my operating system compatible with SARL?

SARL is based on a part of the Eclipse API. Every operating system which has a compatible Java 
Virtual Machine with Eclipse may be used to run SARL. 


### What version of the Java virtual machine is required?

SARL requires the JRE and the JDK [:compiler.level!] or higher to compile and run.
Note that if you plan to create Android applications, you should
configure your JDK to produce 1.6 class files from [:compiler.level!] Java code. 
[:Fact:]("[:compiler.level!]".shouldBeAtLeastJava)


### Why does SARL display an error on startup?

Most of the time the problem is due to an incompatibility between
the configuration of your operating system or Java virtual machine,
and the SARL Eclipse product.

If a problem occured, find the ".log" file in which Eclipse is writting
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


### Why does the SARL product launch but not contain any features related to SARL?

This is due to a problem in your configuration. SARL tools need the Eclipse
framework to be run with a Java Development Kit [:compiler.level!] or higher.
You are currently running the SARL product with a lower version of the JDK.

You must run the SARL product with a valid version of the JDK.
Two ways are available for solving this issue:

1. install the JDK [:compiler.level!], and configuring your operating system to use it by default; or
2. force the SARL product to use the JDK [:compiler.level!] by editing the `eclipse-sarl.ini` file. Add the following parameter on a new line: `-vm path`, where `path` is the path to the binary file `javaw[.exe]` or `java[.exe]` of the JDK [:compiler.level!].
[:Fact:]("[:compiler.level!]".shouldBeAtLeastJava)


### Why does the content assistant not propose any suggestion on MacOS?

By default in the SARL product, the shortcut key for invoking the content assistant is `Ctrl+Space` or `Apple+Space`.
But, the Spotlight tool on MacOS is also using the same shortcut key.

For enabling the SARL product content assist, we recommend to change its shortcut in the SARL product. Go to:

* Window > Preferences
* General > Keys
* Content Assist
* Change the shortcut key

![Content assist shortcut change](./ctrlspaceprefs.png)


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

