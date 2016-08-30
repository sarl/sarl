Demo: Distributed Factorial
===========================

## Principle of the Demo

The `FactorialAgent` agent is waiting for
events that are querying the computing of the factorial of the value embedded in the event.

The `FactorialQueryAgent` agent is querying to the
previous agent to compute the factorial of the value
given as the first parameter of the `FactorialQueryAgent`
agent.

The two agents are using a specific capacity (and its
associated skill) for logging messages.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo, you need to launch two agents
in two different Janus runtime environments.
Type on the two following command lines in different terminals:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.factorial.distributed.FactorialAgent

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.factorial.distributed.FactorialQueryAgent 5

The value `5` is the first parameter, and the agent is
computing `5!`.
