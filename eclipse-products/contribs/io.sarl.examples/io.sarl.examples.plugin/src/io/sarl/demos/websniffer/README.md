Demo: Web Sniffer
=================

## Principle of the Demo

This demo provides a simple web sniffer, which
is intercepting the messages between the Janus
runtime environments, and output information on
a web interface.

**This demo contains a companion Agent for the web sniffer demo.**
It only emits periodic event on the default space of default context.

The web sniffer demo is a web-based application that is
available on [Github](https://github.com/janus-project/janus-web-sniffer).  

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the companion agent, you need to type
the following command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.websniffer.Ticker
