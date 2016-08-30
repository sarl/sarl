Demo: Count Down
================

## Principle of the Demo

The agent is starting, saying "Hello World!", 
it displays "Wake up!" every second, and
counting down from 6 to 0 every two seconds.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.basic.countdown.CountDownAgent
