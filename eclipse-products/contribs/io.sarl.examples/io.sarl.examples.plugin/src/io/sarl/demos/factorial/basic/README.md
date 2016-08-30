Demo: Basic Factorial
=====================

## Principle of the Demo

The `FactorialAgent` agent is computing the factorial
value of its first parameter (given on the command line).

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo, you need to launch the agent
in a Janus runtime environments.
Type the following command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.factorial.basic.FactorialAgent 5

The value `5` is the first parameter, and the agent is
computing `5!`.
