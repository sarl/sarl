# Assertion Support

An assertion is a statement that a predicate is expected to always be true at that point in the code.
If an assertion evaluates to false at run time, an assertion failure results, which typically causes
the program to crash, or to throw an assertion exception.

## Assert Statement

SARL supports assertions in the source code by the [:assertkw:] keyword.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var someCondition : boolean
			[:On]
			[:assertkw](assert) [:someCondition](someCondition)
			[:Off]
		}
	}
[:End:]


The [:someCondition:] expression is the Boolean expression that is dynamically evaluated.
For example, in the following code, the two first [:assertkw:] have their conditions evaluated to true, and do not stop the program.
The third [:assertkw:] condition is evaluated to false. It causes a stop of the program.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var x : int
			[:On]
			x = 1
			[:assertkw!] x > 0
			x++
			[:assertkw!] x > 1
			[:assertkw!] x <= 1
			[:Off]
		}
	}
[:End:]


## Error Message

Sometimes, it is useful to give an explanation about the failure.
The [:assertkw:] keyword accepts a string of character that is the message given when the program has crashed.
The message string follows the condition, with a coma character between them.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def example {
			var someCondition : boolean
			[:On]
			[:assertkw](assert) [:someCondition](someCondition), "the failure explanation is here"
			[:Off]
		}
	}
[:End:]

		
# Enable and Disable Assertions.

By default, assertions are disabled. It means that they are not evaluated at run-time.
In this way, if someone is launching your application, the application is running quickly, and
the assert errors cannot occur. It is compliant with the fact that assertions are tests by and for
the application's developers: the final user of the application is not care about the implementation
constraints.

For enabling and disabling the assertions, you must change the launch configuration within the SARL product,
or use an option that is provided by the SARL run-time environment.

## Inside the SARL Development Environment

Within the SARL development environment, you must define a launch configuration for running a SARL application.
Two launch configurations are available:
* "SARL Agent" for launching an agent, and
* "SARL Application" for launching a standard application (based on the definition of a `main` method), which will launch the SARL framework later. 

Both of these launch configurations allow you to enable or disable the assertions.
On the figure below, you could see the "Enable assertions in run mode" and "Enable assertions in debug mode"
checkboxes within the "Launch Options" section.
If the first (resp. second) box is checked, assertions will be enabled when you application was launched in run (resp. debug) mode. 

![Screenshot of the dialog box of a SARL launch configuration](./enableassertions.png)

## Launching the Janus framework 

The Janus framework is one implementation of a SARL run-time environment. You have to use one of its command-line
options for enabling assertions. All the command-line options are detailed on a [dedicated page](../../tools/Janus.md).
Usually, the option for enabling the assertions is the same as the one of the Java virtual machine: [:shorteaopt!].

## Launching the Java virtual machine 

If you want to run your SARL application by launching the Java virtual machine, you could use the standard command
line options [:longeaopt:] and [:shorteaopt:] that are provided by this virtual machine:

```text
java [ [:longeaopt](-enableassertions) | [:shorteaopt](-ea)  ] <class name>
```



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
