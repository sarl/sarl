Demo: Inheritance Example
=========================

## Principle of the Demo

The agent is displaying "Hello World in the child agent!".
After 2 seconds, it is displaying "Kill myself" and 
is commiting a suicide.

The launched agent is inheriting a part of his behavior from
a super agent.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.basic.inheritance.HelloChildAgent
