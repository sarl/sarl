# General Syntax Reference of he SARL Language

[:Outline:]

SARL is designed to facilitate the creation of agent-based systems. It combines agent-specific constructs with several functional, imperative and object-oriented principles, providing a robust framework for developing autonomous and interactive agents. Below is an overview of the core features of SARL.

## Basic Language Constructs

### Structure of SARL Scripts

- [Script](./Script.md): SARL scripts are the basic units of execution, containing a series of types, statements and expressions.
- [Naming Convention](./Names.md): SARL has specific rules and conventions for naming variables, functions, and other constructs.
- [Statements](./Statements.md): Statements in SARL are used to perform actions, including variable declarations, control flow, and more.
- [Block Documentation](./Block.md): Blocks in SARL define scopes for variables and control structures.

### Variables and Types

- [Variable Declarations](./VarDecls.md): SARL supports various ways to declare variables, including type inference and explicit typing.
- [Types](./Types.md): SARL provides a rich type system, including primitive types, custom types, and collections.
- [Cast Operator](./Cast.md): SARL supports type casting, allowing for explicit conversion between types.

### Literals and Operators

- [Literals](./Literals.md): These are fixed values that can be directly used in code, such as numbers, strings, and booleans.
- [Operators](./Operators.md): SARL supports a wide range of operators for arithmetic, logical, and relational operations.

### Feature Access

- [Member Access](./MemberAccess.md): This feature allows access to the members (fields and methods) of objects and classes.
- [Java Interoperability](./JavaInterroperability.md): SARL is designed to interoperate seamlessly with Java, allowing for the integration of Java libraries and frameworks.

## Control Flow

### Conditional Statements

- [If Expression](./IfExpression.md): Conditional logic in SARL is handled through `if` expressions, allowing for branching execution paths.
- [Switch Expression](./SwitchExpression.md): Switch expressions provide a concise way to handle multiple conditional branches.

### Loops

- [Loop Expressions](./LoopExpression.md): SARL supports various loop constructs for iterative execution, including for, while, and do-while loops.

## Functions and Lambda Expressions

### Function Declarations

- [Function Declarations](./FuncDecls.md): Functions in SARL can be declared to encapsulate reusable pieces of code.

### Lambda Expressions

- [Lambda](./Lambda.md): Lambdas provide a concise way to define anonymous functions, facilitating functional programming patterns.

## Advanced Features

### Error Handling and Code Quality Improvement

- [Exceptions](./Exception.md): SARL includes mechanisms for handling exceptions, allowing for robust error management.
- [Assertions](./Assertion.md): Assertions are used to validate assumptions within the code, aiding in debugging and testing.

### Synchronization of Resources

- [Synchronization](./Synchronization.md): SARL provides constructs for synchronizing access to shared resources, ensuring parallel execution (thread) safety.

### Extensions and Metaprogramming

- [Extension Methods](./Extension.md): SARL supports extension methods to enhance the functionality of types.
- [Active Annotations](./ActiveAnnotations.md): Annotations in SARL can be used to add metadata and influence the behavior of agents.
- [Synthetic Functions](./SyntheticFunctions.md): These are automatically generated functions that facilitate common patterns and reduce boilerplate code.


[:Include:](../../includes/oopref.inc)
[:Include:](../../includes/legal.inc)
