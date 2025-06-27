# Comparison between SARL and Other Languages

<a href="https://en.wikipedia.org/wiki/Java_(programming_language)">Java</a>, [Xtend](https://www.eclipse.org/xtend/) and
[Scala](http://scala-lang.org/) are object-oriented programming languages.
As SARL, Xtend is based on the [Xtext](https://www.eclipse.org/Xtext/) libraries for compiling to the Java language.

The main features coming from the Java language are supported by SARL too. The following table provides the major
differences between the SARL, Java, Xtend and Scala languages, excluding any feature provided by the development
environment (Eclipse, IntelliJ...)

## Agent-Oriented Programming Features


|                                                                               |SARL |Java|Xtend| Scala                   |
|:----------------------------------------------------------------------------- |:---:|:--:|:---:|:-----------------------:|
| [Agent, Capacity, Skill, Behavior...](./aop/index.md) | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span>  | <span style="color: orange;">Partial: actor paradigm</span> |



## Object-Oriented Programming Features


|                                                                               |SARL |Java|Xtend| Scala                   |
|:----------------------------------------------------------------------------- |:---:|:--:|:---:|:-----------------------:|
| [Definition of class and interface types](./oop/index.md)                     | Yes |Yes | Yes | Yes                     |
| [Object-oriented enumeration](./oop/Enumeration.md)                           | <span style="color: orange;">No, only constants could be defined</span> | Yes, constants and functions could be defined | <span style="color: red;">No, only constants could be defined</span> | Yes, constants and functions could be defined |
| [Definition of annotation types](./oop/AnnotationType.md)                         | Yes |Yes | Yes | Yes                     |
| [Definition of static constructors](./oop/Class.md#static-constructor-definition)   | Yes | Yes | <span style="color: red;">No</span> | <span style="color: orange;">See companion object</span> |
| Inheritance of constructors                                                   | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: red;">No</span> |
| [Automatic creation of read-only data structure](./expr/ActiveAnnotations.md#data) | <span style="color: orange;">Manual with `@Data` annotation</span> | <span style="color: red;">No</span> | <span style="color: orange;">Manual with `@Data` annotation</span>| <span style="color: red;">No</span> |
| [Automatic creation of getters and setters](./expr/ActiveAnnotations.md#accessors) | <span style="color: orange;">Manual with `Accessors` annotation</span> | <span style="color: red;">No</span> | <span style="color: orange;">Manual with @Accessors annotation</span> | Yes |
| Automatic creation of final-field constructor                                  | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: orange;">Manual with `@FinalFieldsConstructor` annotation</span> | <span style="color: red;">No</span> |
| Automatic creation of [equals()](https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#equals-java.lang.Object-)
            and [hashCode()](https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#hashCode--) | Yes | <span style="color: red;">No</span> | <span style="color: orange;">Manual with @EqualsHashCode annotation</span> | Yes, see case class |
| Automatic creation of [clone()](https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#clone--) when cloneable type | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | Yes |
| Automatic creation of a serialVersionUID field when
        [serializable type](https://docs.oracle.com/javase/8/docs/api/java/io/Serializable.html) | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: orange;">Manual with `@SerialVersionUID`</span> |
| [Automatic creation of the toString() function](./expr/ActiveAnnotations.md#tostring) | <span style="color: orange;">Manual with `@ToString` annotation</span> | <span style="color: red;">No</span> | <span style="color: orange;">Manual with `@ToString` annotation</span> | Yes, see case class |



## Functions, Procedures and Operators


|                                                                               |SARL |Java|Xtend| Scala                   |
|:----------------------------------------------------------------------------- |:---:|:--:|:---:|:-----------------------:|
| [Definition of dispatch functions](./expr/FuncDecls.md#dispatch-function) | Yes | <span style="color: red;">No</span> | Yes | <span style="color: red;">No</span> |
| [Extension methods](./expr/Extension.md) | Yes | <span style="color: red;">No</span> | Yes | Yes |
| [Definition of variadic functions](./expr/FuncDecls.md#variadic-function) | Yes | Yes | Yes | Yes |
| [Definition of default values for the formal parameters](./expr/FuncDecls.md#default-value-for-the-formal-parameters) | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | Yes |
| [Operator overloading](./expr/Operators.md#operator-overloading) (except assignment and casting, see below) | Yes | <span style="color: red;">No</span> | Yes | Yes |
| [Cast operator overloading](./expr/Cast.md) | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: red;">No</span> |
| [Assignment operator overloading](./expr/Operators.md) | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: red;">No</span> |
| Automatic detection of [pure functions](http://download.eclipse.org/modeling/tmf/xtext/javadoc/2.9/org/eclipse/xtext/xbase/lib/Pure.html) and marking | Yes | <span style="color: red;">No</span> | <span style="color: red;">No</span> | <span style="color: red;">No</span> |



## Code Expressions


|                                                                               |SARL |Java|Xtend| Scala                   |
|:----------------------------------------------------------------------------- |:---:|:--:|:---:|:-----------------------:|
| [Definition of lambda expressions](./expr/Lambda.md) | Yes | Yes | Yes | Yes |
| [Inference of types](./expr/VarDecls.md#typing) | Yes | <span style="color: red;">No</span> | Yes | Yes |
| [Support the break statement](./expr/LoopExpression.md#breaking-a-loop) | Yes | Yes | <span style="color: red;">No</span> | Yes |
| [Support the continue statement](./expr/LoopExpression.md#jump-to-the-next-iteration) | Yes | Yes | <span style="color: red;">No</span> | Yes |
| [Implicit typecasting between number values](./expr/Cast.md#implicit-conversions), including `AtomicInteger`, `AtomicLong`, `AtomicDouble`, `BigInteger`, and `BigDecimal` | Yes | <span style="color: orange;">Partial: [primitive](./expr/Types.md#primitive-types) to [primitive types](./expr/Types.md#primitive-types), primitive to [object wrapper types](./expr/Types.md#primitive-types), and [object wrapper](./expr/Types.md#primitive-types) to [primitive types](./expr/Types.md#primitive-types)</span> | <span style="color: orange;">Partial: [primitive](./expr/Types.md#primitive-types) to [primitive types](./expr/Types.md#primitive-types), primitive to [object wrapper types](./expr/Types.md#primitive-types), and [object wrapper](./expr/Types.md#primitive-types) to [primitive types](./expr/Types.md#primitive-types)</span> | Yes |
| [Arithmetic operations with any type of number as operand](./expr/Operators.md), including `AtomicInteger`, `AtomicLong`, `AtomicDouble`, `BigInteger`, and `BigDecimal` | Yes | <span style="color: orange;">Partial: [primitive](./expr/Types.md#primitive-types) and [wrapper types](./expr/Types.md#primitive-types) | Partial: primitive and [wrapper types](./expr/Types.md#primitive-types) | Yes |



## References

This documentation is based on elements from the following sources:

* [Xtend](https://www.eclipse.org/xtend/documentation.html)
* [Xtext](https://www.eclipse.org/Xtext/documentation.html)
* [Java Tutorials](https://docs.oracle.com/javase/tutorial/)
* [Scala Home Page](https://www.scala-lang.org/)



[:Include:](../includes/legal.inc)
