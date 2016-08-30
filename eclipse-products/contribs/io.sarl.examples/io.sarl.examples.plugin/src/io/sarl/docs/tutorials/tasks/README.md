Tutorial: Dependent Tasks
=========================

## Principle of the Demo

The principle of the application is the following:

* Let consider a collection of 5 Java source files.
* The dependency tree of these files are: 
        File_1.java
        +- File_1_1.java
        |  +- File_1_1_1.java
        |  \- File_1_1_2.java
        \- File_1_2.java
* The initializing agent creates a task-manager agent for each of the files.
* The initializing agent asks to the task-manager agent associated to the `File_1.java` file.
* Each task-manager agent asks to the task-manager agents of the dependency files to run the compilation.
* When the dependency files are compiled, the task-manager agent is compiling its associated file.
   
## Compiling the Demo using Maven

You need to compile the demo with Maven. Type on the command
line:

> mvn clean package

## Launching the Demo

For launching the demo in a single Janus runtime environment
instance, type on the command line:

> mvn exec:java
>     -Dexec.mainClass=io.janusproject.Boot
>     -Dexec.args=io.sarl.docs.tutorials.tasks.TaskInitializer
