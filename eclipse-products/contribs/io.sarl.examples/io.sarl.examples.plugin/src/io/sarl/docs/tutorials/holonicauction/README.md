Tutorial: Holonic Auction
=========================

## Principle of the Demo

The principle of the application is the following:

* The super-agent creates 3 sub-agents that are computing an integer number.
* After the sub-agents have given their numbers to the super-agent, this last is displaying the ID of the agent that has win (with the higher number).
   
By a design choice, the players are the sub-agents. 

## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.docs.tutorials.holonicauction.Auctioneer
