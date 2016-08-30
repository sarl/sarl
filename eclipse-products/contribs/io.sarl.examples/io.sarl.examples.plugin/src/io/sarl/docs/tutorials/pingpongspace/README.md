Tutorial: Agent Communication in Subspace
=========================================

## Principle of the Demo

The principle of the application is the following:

* The Ping agent is sending a Ping message to all agents in a specific space.
* The Pong agent is receiving the Ping message, and replies with a Pong message to the sender of the Ping message.
* The Ping agent is receiving a Pong message and replies to the sender of the Pong with a new Ping message.

These messages contains an integer number that indicates the number of the event.

The messages are exchanged into a subspace of the default context.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.docs.tutorials.pingpongspace.BootAgent
