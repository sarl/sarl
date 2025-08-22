# Variable and Field Declaration

[:Outline:]

Variables and fields can be declared in SARL. To declare a variable or a field, you must specify if it is a value or a
variable (see below for details), its name, and optionally its type and its initial value.

The variable/value declaration follows the syntax:

```text
[:varkw](var) NAME [: TYPE] [= INITIAL VALUE]
[:valkw](val) NAME [: TYPE] [= INITIAL VALUE]
```

Shadowing variables from outer scopes is not allowed, the only exception is the implicit variable `[:it](it)`.


## Variable vs. Value Declaration

A variable declaration starting with the keyword [:valkw:] denotes a value, which is essentially a final, unsettable variable.

[:Failure:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			val a = 5
			a = 7
		}
	}
[:End:]


The variable needs to be declared with the keyword [:varkw:], which stands for 'variable', if its value can change.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			var a = 5
			a = 7
		}
	}
[:End:]


Variables declared outside a lambda expression using the [:varkw:] or [:valkw:] keyword are accessible from within the
lambda expressions.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action1 : () => int {
			var a = 5
			[ a + 5 ]
		}
		def action2 : () => int {
			val a = 5
			[ a + 5 ]
		}
	}
[:End:]


Fields declared outside a lambda expression using the [:varkw:] keyword or the [:valkw:] keyword are
accessible from within the lambda expressions.

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		var a = 5
		def action : () => int {
			[ a + 5 ]
		}
	}
[:End:]


## Typing

The type of the variable itself can either be explicitly declared or it can be inferred from the initializer expression.

In the following example, the type of the variable is explicitly given:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			[:On]
			var a : [:vartype](String) = "abc"
			[:Off]
			a
		}
	}
[:End:]


In the following example, the type of the variable is inferred to [:vartype:]:

[:Success:]
	package io.sarl.docs.reference.gsr
	agent A {
		def action : Object {
			[:On]
			var a = "abc"
			[:Off]
			a
		}
	}
[:End:]


## Implicit or Anaphoric Variables: this, super, it and occurrence

In the domain of computer science and programming language theory, an [anaphoric macro or variable](https://en.wikipedia.org/wiki/Anaphoric_macro) refers to a linguistic construct that enables the implicit capture and reuse of intermediate computation results within an expression. The term anaphora originates from linguistics, where it denotes a word or phrase that refers back to a previously mentioned entity. In programming, this concept is adapted to allow a macro or variable to automatically bind to the result of a preceding computation, thereby reducing redundancy and enhancing expressiveness.
Anaphoric macros are most commonly associated with Lisp-family languages and domain-specific languages that support advanced macro systems.
The core characteristics of anaphoric variables are:

- **Implicit Binding:** The variable does not require explicit assignment; instead, it is bound to the result of the most recent relevant computation.
- **Lexical Scoping:** The binding is typically confined to the lexical scope of the variable use, ensuring referential transparency and avoiding unintended side effects.
- **Abstraction:** Anaphoric constructs abstract away repetitive patterns, such as repeatedly referencing the result of a function call or expression.


The following table describes the four anaphoric variables that are defined in SARL.


| Name of the implicit variable | Operational Semantic | Implicitly in naming scope w/ priority level |
|-------------------------------|----------------------|----------------------------------------------|
| `this`                        | Reference to the current instance of agent, behavior, skill, class or enumeration. The type of `this` is the type where it is mentionned in the code | Yes (2) |
| `super`                       | Reference to the current instance of agent, behavior, skill, class or enumeration. The type of `this` is the super-type of the type where it is mentionned in the code | Yes (3) |
| `it`                          | Reference to an object that depends on the current of usage in the code according to the concept of the [anamorphic macros or variables](https://en.wikipedia.org/wiki/Anaphoric_macro). To determine if `it` is defined at a specific locaiton in the code, you have to refer to the document of the enclosing features | Yes (1) |
| `occurrence`                  | Reference to the current instance of an event received by an agent, behavior or skill | No |



Like in Java the current object is bound to the keyword `this`. This allows for implicit field access or method invocations.
For example, in the following code, `this` represents the current agent instance in memory. Therefore, the code is changing the value of the agent's field [:thisexamplefield:]. In this code, the type of `this` is `A`.

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	agent A {
		var [:thisexamplefield](myvariable) : int
		def action(value : int) {
			this.myvariable = value
		}
	}
[:End:]


In the following code, `super` represents the current agent instance in memory but with a type different than `this`: `super` has the super-type, here `B`. `super` is equivalent to `this as B` in this example.

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	agent B {
		protected var mysupervariable : int
	}
	agent A extends B {
		var thisexamplefield : int
		def action(value : int) {
			super.mysupervariable = value
		}
	}
[:End:]



The `it` variable is defined and binded by some SARL constructs. For example, Let a [lambda expression][./Lambda.md] declared as a formal formater of a [:itexamplefct:]. This lambda expression takes one argument:

[:Success:]
	package io.sarl.docs.reference.gsr
	class A {
	[:On]
		static def [:itexamplefct](function)(parameter : (int) => boolean) {
			println(parameter.apply(45))
		}

		static def [:itexamplemain](main) {
			function [ p | p > 30 ]
			function [ it > 30 ]
		}
	[:Off]
	}
[:End:]

The two lines in the [:itexamplemain:] function are equivalent. On the first line, the formal parameter of the lambda expression is explicitly named. On the second line, the formal parameter is not explicitly named. Therefore, the SARL compiler assumes that this parameter becomes the anphoric variable `it` with the value given as argument to the lambda expression.

SARL also defines the `it` variable in the [guards of the agent's behaviors units](../aop/Agent.md#behaviors-of-an-agent). The three guards in the following code are equivalent. Here `it` is binding to the `occurrence` variable. The `occurrence` variable is the forth anaphoric variable supported by SARL. It is binded to the instance of a received event.

[:Success:]
	package io.sarl.docs.reference.gsr
	[:On]
	event E {
		var attr : int
	}
	
	agent A {
		on E [occurrence.attr > 10] {
		}

		on E [it.attr > 10] {
		}

		on E [attr > 10] {
		}
	}
[:End:]



[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
