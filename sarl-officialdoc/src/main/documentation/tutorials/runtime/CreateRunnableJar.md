# Create a runnable application (Jar) file for SARL application

[:Outline:]

Usually, it's convenient to bundle many SARL entity files into a single archive file. This file is named runnable application file (a.k.a. runnable jar file in the Java community).
It allows a use to run SARL entities without having to know their names and type them in a command prompt, rather the user can just double click on the runnable application file and the program will fire up.
A runnable application file allows SARL entities to be loaded just like when a user clicks an regular executable file.

In this tutorial, we're going to cover the key points for creating a runnable application file for SARL.
Specifically, we'll take a simple application and explore the ways to package and run it as a runnable file.


## Preparing the SARL project

Before creating the runnable application file, it is necessary to have a SARL project and its associated run configuration.

### SARL Project Sample

For creating a runnable application file, you must create or have a SARL project with a "main program" as it is illustrated in the following figure.

![Example of SARL program](export_program_example.png)

The mandatory element in your program must be the [:main:] function:

[:Success:]
	package io.sarl.docs.tutorials.createrunnablejar
	class MainProgram {
	[:On]
		static def [:main](main)(args : String[]) : void {
		}
	[:Off]
	}
[:End:]

You could find a description of the method for [creating your first SARL project](../../getstarted/CreateFirstProject.md).


### SARL Application Run Configuration

Once you have implemented your SARL program, you must define a run configuration.
This configuration is a feature of the SARL IDE that enables to define a method for
[launching the program within the SARL Eclipse IDE](../../getstarted/RunSARLAgentEclipse.md).

For defining the run configuration, you should follow the steps:

1. Open the dialog box for the run configurations.

![Open run configurations](./export_menu_runs.png)

2. Create a SARL Application, as illustrated in:

![Create a SARL application](./export_runs_config.png)


## Creating the runnable application file

To create a new runnable application file in the workbench:

* From the menu bar's File menu, select Export.

![Open the export wizard](./export_menu_file_export.png)

* Expand the SARL node and select "Runnable SARL application". Click Next.

![Select the type of export](./export_wizard_type.png)

* In the wizard page, select your run/launch configuration to use to create a runnable file.

![Fill up the export parameters](./export_configuration.png)

* In the Export destination field, either type or click Browse to select a location for the JAR file that will contain the runnable application
* Run the exporter.


[:Include:](../../includes/legal.inc)
