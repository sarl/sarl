Demo: Counter Agent
===================

## Principle of the Demo

An agent is counting the number of seconds from 0 to 3 when
another agent is appearing in the default space.
When the agent is arriving at the end of the count, it is
killing itself. 

The `CounterAgent` agent is counting. 
The `SecondAgent` agent doing nothing special.

Note: the `SecondAgent` agent is not stopping. You must
kill the Janus runtime environment yourself.

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
>     -Dexec.args=io.sarl.demos.counter.CounterAgent

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.counter.SecondAgent
