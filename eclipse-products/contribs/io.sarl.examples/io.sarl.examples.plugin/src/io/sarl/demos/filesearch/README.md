Demo: Distributed File Search
=============================

## Principle of the Demo

A set of agents of type `FileSearchAgent` is running
on different Janus runtime environment over the network.
Each of them is able to search for a file in the local
hard disk.

The agent of type `SearchCommanderAgent` is the agent
that is querying to search for a specific file.

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo, you need to launch at least two agents
in at least two different Janus runtime environments.
Type on the following command lines in different terminals
(the order of the command lines is important):

For each searching agents:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.filesearch.FileSearchAgent path

The parameter `path` is the name of the folder in which the
agent is searching for files.

For the commanding agent:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.demos.filesearch.SearchCommanderAgent .txt

The parameter `.txt` is the filename extension of the files 
that are matching during the search.
