Demo: Holarchy Example
======================

## Principle of the Demo

A holon is an agent that is composed of other holons.
The demo is creating a hierarchy of holons, destroying
this hierarchy after few seconds.

The agent `Holon` is defined as following:

* if the holon is at depth upper than 3, it is spawning 2 sub-holons of the `Holon` type.
* if the holon is at depth 3, it is waiting 10 seconds and commit a suicide.

The agent `HolonManager` is the start-up agent that is
initializing the root holon.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.basic.holarchy.HolarchyManager
