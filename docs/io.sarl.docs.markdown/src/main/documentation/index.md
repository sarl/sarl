# SARL Documentation

[:Outline:]

## Getting Started

* [Install SARL Tools](./gettingstarted/InstallSARLTools.md)
* [Create First Project](./gettingstarted/CreateFirstProject.md)
* [Agent Definition Introduction](./gettingstarted/AgentIntroduction.md)
* [Run SARL Agent in the Eclipse IDE](./gettingstarted/RunSARLAgentEclipse.md)
* [Run SARL Agent from the Command Line](./gettingstarted/RunSARLAgentCLI.md)
* [Run SARL Agent from a Java Program or a SARL class](./gettingstarted/RunSARLAgentJava.md)

## Frequently Asked Questions (FAQ)

* [General FAQ on SARL](./faq/GeneralFAQ.md)
* [SARL Syntax FAQ](./faq/SyntaxFAQ.md)
* [Runtime Environment FAQ](./faq/RuntimeEnvironmentFAQ.md)
* [What are the deprecated and removed features?](./faq/DeprecatedAPI.md)

## Tutorials

### Event Communication

* [Agent Communication with the ping-pong agents](./tutorials/PingPong.md)
* [Agent Communication in sub-space with the ping-pong agents](./tutorials/PingPongSpace.md)
* [Override the agent event bus with the Behaviors capacity](./tutorials/EventBusOverrideWithCapacity.md)

### Parallel Execution

* [Parallel execution within the agents](./tutorials/ParallelExecution.md)
* [Initialization of a multiagent system](./tutorials/MASInitialization.md)

### Organizational Patterns

* [English Auction with Holons](./tutorials/HolonicAuction.md)

### SARL Run-time Environment

* [Creating an extension for the Janus SRE](./tutorials/SreExtension.md)

## Best Practices with SARL

* [Event Creation with a Builder](./bestpractices/EventBuilder.md)
* [Event Handler Overriding](./bestpractices/EventHandlerOverriding.md)
* [Creating Space with Operation User Accessibility](./bestpractices/SpaceWithCallerIdentity.md)

## Reference Documentation

### General Syntax

* [General Syntax Reference](./reference/GeneralSyntax.md)
* Structural elements;
	* [Script format](./reference/general/Script.md)
	* [Function declaration](./reference/general/FuncDecls.md)
	* [Synthetic Functions](./reference/general/SyntheticFunctions.md)
* Type system:
	* [Supported Types for Variables and Parameters](./reference/general/Types.md)
* Constant expressions:
	* [Numerical, string, and collection literals](./reference/general/Literals.md)
* Structuring expressions:
	* [Block expression](./reference/general/Block.md)
	* [If-then-else expression](./reference/general/IfExpression.md)
	* [Switch expression](./reference/general/SwitchExpression.md)
	* [Loops](./reference/general/LoopExpression.md)
	* [Lambda expressions](./reference/general/Lambda.md)
* Standard expressions:
	* [Variable and attribute declarations](./reference/general/VarDecls.md)
	* [Operators](./reference/general/Operators.md)
	* [Type casting](./reference/general/Cast.md)
	* [Access to object members](./reference/general/MemberAccess.md)
	* [Extension methods](./reference/general/Extension.md)
	* [Synchronization expression](./reference/general/Synchronization.md)
* Errors and exceptions:
	* [Exceptions](./reference/general/Exception.md)
	* [Assertions](./reference/general/Assertion.md)
* Meta-programming:
	* [Active annotations](./reference/general/ActiveAnnotations.md)
* [Compiler Errors](./reference/CompilerErrors.md)
* [Comparison between SARL and other Languages](./reference/LanguageComparison.md)

### Agent-Oriented Programming

* [Event Reference](./reference/Event.md)
* [Capacity Reference](./reference/Capacity.md)
* [Skill Reference](./reference/Skill.md)
* [Agent Reference](./reference/Agent.md)
* [Behavior Reference](./reference/Behavior.md)
* [Built-in Capacity Reference](./reference/BIC.md)
* [Space Reference](./reference/Space.md)
* [Management of the Failures and Validation Errors](./reference/Failures.md)
* [Compiler Errors](./reference/CompilerErrors.md)

### Object-Oriented Programming

* [Basic Object-Oriented Programming Support](./reference/OOP.md)
* [Comparison between SARL and other Languages](./reference/LanguageComparison.md)
* [Compiler Errors](./reference/CompilerErrors.md)

### SARL Development Toolkit (SDK) and Programming Interface (API)

* [Programmatic Access to the SARL Run-time Environment](./api/SRE.md)
* [Naming and Namespaces](./api/Naming.md)
* [SRE Observation and Probes](./api/Probing.md)

## Compilation and Generation Infrastructure

* [Compiler Errors](./reference/CompilerErrors.md)
* [Basics of the SARL Compilation Process](./compilation/Basics.md)
* [Generation to the Python Language](./compilation/PythonGeneration.md)

## Execution and Run-Time Environment

### Run-time Behavior of SARL Programs

* [Run-time Behavior of SARL Programs](./reference/Runtime.md)

### Janus SRE

* [Janus SRE Official Page](http://www.sarl.io/runtime/janus/index.html)
* [Creating extensions for Janus](./tutorials/SreExtension.md)
* Official Janus Extensions:
  * [Connecting Janus nodes over a computer network](./tools/JanusNetworkExtension.md)
* Command-line Tools:
  * [janus](./tools/Janus.md): launching SARL agents with the Janus platform.
  * [janusnode](./tools/Janusnode.md): launching SARL agents with the Janus platform over a computer network.

## Other Tools

### Syntax Highlightning

* [Syntax highlighting styles for SARL inside other tools](./tools/SyntaxHighlightning.md)

### Apache Maven

* [maven-sarl-plugin](./tools/MavenSarlPlugin.md): the Maven plugin for the SARL compiler.

### Command Line Tools

* [sarlc](./tools/Sarlc.md): command-line SARL compiler.
* [sarldoc](./tools/Sarldoc.md): command-line SARL documentation generator.

## Documentation Format

* [Specific Markdown format for the documentation](./DocumentationContribution.md)


[:Include:](./legal.inc)

