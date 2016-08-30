Demo: Task Scheduling
=====================

## Principle of the Demo

This demo illustrates the creating and the execution
of agent tasks. An agent task is a part of code that
is asynchronously run.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo, you need to launch an agent
in a Janus runtime environments.
Type on the following command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.schedule.ScheduleAgent
