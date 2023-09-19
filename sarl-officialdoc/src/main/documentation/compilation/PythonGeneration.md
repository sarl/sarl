# Generation to the Python Language

[:Outline:]

Python is a widely used high-level programming language for general-purpose programming, created by Guido van Rossum.
As an interpreted language, Python has a design philosophy that emphasizes code readability, and a syntax that allows
programmers to express concepts in fewer lines of code than might be used in languages such as C++ or Java.

## Integration to the Standard Compilation Process

The Python generator for SARL compiler is included into the SARL tool-chain, as illustrated by the following figure.

![Standard Compilation Process for SARL Programs](./compilation_process.png)

## Enabling and Configuring the Python Generator

For enabling and configuring the Python generator, you must open the dedicated preference page into the SARL development environment.
It is accessible by:

* opening the menu item: `Window > Preferences`.
* opening the preference section: `SARL > Compiler > Python`  

You should show a dialog box similar to the following figure.

![Preference Page into the SARL Development Environment](./python_generator_config.png)

This preference page will enable you to:

* enable and disable the Python generation;
* configure the Python generator;
* map the Java typenames to their Python equivalents;
* map the Java feature calls to their Python equivalents.



[:Include:](../legal.inc)

