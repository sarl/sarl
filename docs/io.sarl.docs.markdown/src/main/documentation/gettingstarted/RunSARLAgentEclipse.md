# Run SARL Agent in the Eclipse IDE

[:Outline:]

For running an agent, you must launch this agent on the runtime environment.
In this document, we assume that the runtime environment is the [Janus platform](http://www.janusproject.io).

Two major methods are available for launching a SARL application inside the SARL Eclipse IDE:

* Use a [SARL launch configuration](#create-a-sarl-launch-configuration).
* Use a [Java launch configuration](#create-a-java-launch-configuration).

The SARL launch configuration is recommended.


## Create a SARL Launch Configuration

For launching the SARL agents on the runtime environment inside the SARL Eclipse IDE, you must
define a *Run Configuration*.

> **_Very Important Note:_** If your project is Maven-based, you could not use this method for launching your
> application. You must use the [Java launch configuration](#create-a-java-launch-configuration).


### Create a Java application configuration

Open the run configuration dialog box by selecting **Run > Run Configurations**, and create a new SARL
application. You obtain a page similar to:


![SARL Application](./EclipseRunConfiguration_0_0.png)


Change the *name* of the run configuration, and select the *project*, which is containing your agent.

### Specify the agent to execute

The second step is the specification of the agent to launch.
Keep in mind that you can only give one start-up agent to 
the runtime environment. The other agents will be spawned by the
specified start-up agent.

The start-up agent is given in the *Agent qualified name* field of
the *Main* tab. You must enter the fully qualified name
of the agent that must be launched. 


![Agent to Launch](./EclipseRunConfiguration_0_1.png)


At the bottom of this page, you may change configuration options for the runtime environment.

### Add the Janus runtime environment

For running your agent, you must specify a SARL runtime environment.
In this tutorial, we assume that you want to use the [Janus platform](http://www.janusproject.io).

If you don't want to use the Janus platform, you must download the
runtime environment that you want to use, and install it in the SARL Eclipse environment as follow.
You add a **SARL runtime environment** (or SRE) in
the *Runtime environment* tab. You should click on the **Installed SREs** button for
managing the installed runtime environments (or open the corresponding preference page).
After adding the SRE's JAR file, you obtain a dialog box similar to:


![Add Janus](./EclipseRunConfiguration_0_2.png)



### Give parameters to the Agent

It is possible to give arguments to the launched agent.
Indeed, all the arguments given as program arguments
are put in the [:parameters:] attribute of the [:initevent:] event.
This event is fired when the launched agent is started.
[:Fact:]{io.sarl.core.[:initevent](Initialize)}
[:Fact:]{typeof(io.sarl.core.Initialize).shouldHaveField("[:parameters](parameters)")}

The following example gives the values `FirstArgument` and
`SecondArgument` to the launched agent:


![Program Arguments](./EclipseRunConfiguration_0_3.png)


On this page, you could also specify the parameters to give to the SARL runtime environment or
to the Java virtual machine.


## Create a Java Launch Configuration

For launching the SARL agents on the Janus runtime environment inside
the Eclipse IDE, you must define a *Run Configuration*.


This section is dedicated to the definition of a launcher for Java application (the standard and classical
launching configuration in the Eclipse community).


### Create a Java application configuration

Open the run configuration dialog box by selecting **Run > Run Configurations**, and create a new Java
Application. You obtain a page similar to:


![Java Application](./EclipseRunConfiguration_1_0.png)


Change the *name* of the run configuration, and select the *project*, which is containing your agent.


### Add the Janus runtime environment

For running your agent with the Janus runtime environment, you must add the Janus library in the class path.

For adding the Janus library, select the **Classpath** tab, and **User Entries**.
Click on the **Advanced** button. You will be able to select the type of classpath
entry to add. Select "Add Library". 


![Add Library in the classpath](./EclipseRunConfiguration_1_1.png)


Then, you are able to select the library for the Janus runtime environment. 


![Add Janus Library](./EclipseRunConfiguration_1_2.png)


### Specify the SRE Boot agent

You can go back to the *Main* tab, and enter the *Main class*.
The main class **must always be** `io.sarl.sre.boot.Boot`.


![Janus Boot Class](./EclipseRunConfiguration_1_3.png)


### Specify the agent to execute

The last step is the specification of the agent to launch.
Keep in mind that you can give to the Janus runtime environment
only one start-up agent. The other agents will be spawn by the
specified start-up agent.


The start-up agent is given in the *Program arguments* field of
the *Arguments* tab. You must enter the fully qualified name
of the agent that must be launched. 


![Agent to Launch](./EclipseRunConfiguration_1_4.png)


### Give parameters to the Agent

It is possible to give arguments to the launched agent.
Indeed, all the arguments given as program arguments
are put in the [:parameters:] attribute of the [:initevent:] event.
This event is fired when the launched agent is started.
[:Fact:](io.sarl.core.Initialize)
[:Fact:]{typeof(io.sarl.core.Initialize).shouldHaveField("[:parameters](parameters)")}

The following example gives the values `FirstParam` and
`SecondParam` to the launched agent:


![Program Arguments](./EclipseRunConfiguration_1_5.png)



## Retrieve the Command Line Parameters in the Agent

For retrieving the values passed on the command line, you must handle the `Initialize` event, as illustrated
by the following example:

[:Success:]
	package io.sarl.docs.gettingstarted.runsarlagent
	import io.sarl.core.Logging
	import io.sarl.core.Initialize
	[:On]agent MyAgent {
		uses Logging
		on Initialize {
			println("Command line parameters: " + occurrence.parameters)
		}
	}
[:End:]


## What's next?

In the next section, we will learn how to launch your SARL project from the command line.


[Next>](./RunSARLAgentCLI.md)


[:Include:](../legal.inc)
