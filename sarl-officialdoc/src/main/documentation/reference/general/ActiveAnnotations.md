# Active Annotations

[:Outline:]

Active annotations allow developers to participate in the translation process of SARL source code to Java.
An active annotation is just an annotation that is processed by a specific annotation processor during
the compilation of a SARL program.
Depending on the meaning of the active annotation, the generated Java code could be completed by the annotation processor.

SARL comes with ready-to-use active annotations for common code patterns.
The following sections describe there annotations.

## @Accessors

If you want to add getter and or setter methods for your fields, [:accessorsannon:] is your friend.
This annotation can be applied to either object-oriented types and several agent-oriented types.
The agent-oriented types in which you could uses the [:accessorsannon:] annotation are the agents,
the behaviors and the skills.
To uses this annotation, you have to import it as:

[:Success:]
	[:On]
	import [:accessorsannonfqn](org.eclipse.xtend.lib.annotations.Accessors)
	[:Off]
	agent MyAgent {
		[:accessorsannon!] var name : String
	}
[:End:]
[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	behavior MyBehavior {
		[:accessorsannon!] var name : String
	}
[:End:]
[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	capacity MyCap { }
	skill MySkill implements MyCap {
		[:accessorsannon!] var name : String
	}
[:End:]
[:Failure:]
	import org.eclipse.xtend.lib.annotations.Accessors
	event MyEvent {
		[:accessorsannon!] var name : String
	}
[:End:]


Let's a basic example.

[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	class MyClass {
		[:On]
		[:accessorsannon](@Accessors) var name : String
		[:Off]
	}
[:End:]


It is compiled to the code:
[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	class MyClass {
		[:On]
		private var name : String
		 
		public def getName : String {
  			this.name
		}
 		
		public def setName(name : String) : void {
		  this.name = name
		}
		[:Off]
	}
[:End:]


By default, a public getter and a public setter method is created. The [:accessorsannon:] can be configured to tell
that you only want one or the other, and to change the visibility. This is done by passing one or more values of
type [:accessorparamtype:] for representing the visibility categories
as parameters to the annotation: [:accessorpublicgetterparam:], [:accessorprotectedgetterparam:], [:accessorpackagegetterparam:],
[:accessorprivategetterparam:], [:accessorpublicsetterparam:], [:accessorprotectedsetterparam:], [:accessorpackagesetterparam:],
[:accessorprivatesetterparam:], [:accessornoneparam:].

[:Success:]
	import org.eclipse.xtend.lib.annotations.AccessorType
	class MyClass {
		private var t1 = [:accessorparamtype](AccessorType)::[:accessorpublicgetterparam](PUBLIC_GETTER)
		private var t2 = AccessorType::[:accessorprotectedgetterparam](PROTECTED_GETTER)
		private var t3 = AccessorType::[:accessorpackagegetterparam](PACKAGE_GETTER)
		private var t4 = AccessorType::[:accessorprivategetterparam](PRIVATE_GETTER)

		private var t5 = AccessorType::[:accessorpublicsetterparam](PUBLIC_SETTER)
		private var t6 = AccessorType::[:accessorprotectedsetterparam](PROTECTED_SETTER)
		private var t7 = AccessorType::[:accessorpackagesetterparam](PACKAGE_SETTER)
		private var t8 = AccessorType::[:accessorprivatesetterparam](PRIVATE_SETTER)

		private var t9 = AccessorType::[:accessornoneparam](NONE)
	}
[:End:]


You can also use the annotation on class level to do the same for all fields.

Here is a more complex example, that shows how it works:

[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	[:On]
	[:accessorsannon!] class Person {
	  var name : String
	  var firstName : String
	  [:accessorsannon!](PUBLIC_GETTER, PROTECTED_SETTER) var age : int
	  [:accessorsannon!](NONE) var internalField : String
	}
	[:Off]
[:End:]
 

It is compiled to the code:
[:Success:]
	import org.eclipse.xtend.lib.annotations.Accessors
	[:On]
	class Person {
	  private var name : String
	  private var firstName : String
	  private var age : int
	  private var internalField : String
	  
	  public def getName : String {
	    this.name
	  }
	  
	  public def setName(name : String) : void {
	    this.name = name
	  }
	  
	  public def getFirstName : String {
	    this.firstName
	  }
	  
	  public def setFirstName(firstName : String) : void {
	    this.firstName = firstName
	  }
	  
	  public def getAge : int {
	    this.age
	  }
	  
	  protected def setAge(age : int) : void {
	    this.age = age
	  }
	}
	[:Off]
[:End:]

## @Data

The annotation [:dataannon:] will turn an annotated class into a value object class. A class annotated with
[:dataannon:] is processed according to the following rules:

* all fields are final, i.e. they must be declared with [:valkw:],
* getter methods will be generated (if they do not yet exist),
* a constructor with parameters for all non-initialized fields will be generated (if it does not exist),
* equals(Object) / hashCode() methods will be generated (if they do not exist),
* a toString() method will be generated (if it does not exist).

This annotation can be applied to object-oriented types. The agent-oriented types cannot be annotated.

Example:
[:Success:]
	[:On]
	import [:dataannonfqn](org.eclipse.xtend.lib.annotations.Data)
	[:dataannon](@Data) class Person {
	  [:valkw](val) firstName : String
	  val lastName : String

	  static def main(args : String*) {
	    val p = new Person(args.get(0), args.get(1))
	    println(p.getFirstName + ' ' + p.lastName)
	  }
	}
	[:Off]
[:End:]
[:Failure:]
	import org.eclipse.xtend.lib.annotations.Data
	@Data agent MyAgent {
	}
[:End:]

## @Delegate

The [:delegateannon:] annotation automatically generates delegate methods for all interfaces shared between the delegate
and the currently implemented class. You can optionally restrict it to explicitly stated interfaces.
This annotation can be applied to object-oriented types. The agent-oriented types cannot be annotated.

Let's start with a basic example:
[:Success:]
	[:On]
	import [:delegationannonfqn](org.eclipse.xtend.lib.annotations.Delegate)
	interface SomeInterface {
		def function(param : String) : int
	}
	interface SubTypeOfSomeInterface extends SomeInterface {
		def anotherFunction
	}
	class MyClass implements SomeInterface {
	 
	  // generates all methods of SomeInterface and delegates to this field
	  [:delegateannon](@Delegate) var myDelegate : SubTypeOfSomeInterface
	 
	}
	[:Off]
[:End:]
[:Failure:]
	import org.eclipse.xtend.lib.annotations.Delegate
	[:delegateannon!] agent MyAgent {
	}
[:End:]


The previous code is equivalent to:
[:Success:]
	import io.sarl.activeannotation.SomeInterface
	import io.sarl.activeannotation.SubTypeOfSomeInterface
	import io.sarl.activeannotation.MyDelegate
	[:On]
	class MyClass implements SomeInterface {			 
	  var myDelegate : SubTypeOfSomeInterface
	  
	  def function(param : String) : int {
	    return this.myDelegate.function(param)
	  }
	}
	[:Off]
[:End:]


It is not only possible to delegate to fields, but also to methods so you could lazily
create the delegate object or use a different one each time.

[:Success:]
	import org.eclipse.xtend.lib.annotations.Delegate
	import io.sarl.activeannotation.MyDelegate
	import io.sarl.activeannotation.SomeInterface
	[:On]			
	class MyClass implements SomeInterface {
		[:delegateannon!] def provideDelegate : SomeInterface {
			return new MyDelegate
		}
	}
	[:Off]
[:End:]


The previous code is equivalent to:
[:Success:]
	import io.sarl.activeannotation.SomeInterface
	import io.sarl.activeannotation.MyDelegate
	[:On]
	class MyClass implements SomeInterface {
		def function(param : String) : int {
			return provideDelegate().function(param)
		}
	
		def provideDelegate : SomeInterface {
			return new MyDelegate
		}
	}
	[:Off]
[:End:]


If you use a method, additional parameters could be declared, that will tell you about the method that should be invoked:

* the name of the method,
* the types of the formal parameters, and
* the arguments to the method.

Let's the following example:

[:Success:]
	import org.eclipse.xtend.lib.annotations.Delegate
	import io.sarl.activeannotation.MyDelegate
	import io.sarl.activeannotation.SomeInterface
	[:On]
	class MyClass implements SomeInterface {
		[:delegateannon!] def provideDelegate(methodName : String, parameterTypes : Class<?>[], arguments : Object[]) : SomeInterface {
			return new MyDelegate
		}
	}
	[:Off]
[:End:]

The previous code is equivalent to:

[:Success:]
	import io.sarl.activeannotation.SomeInterface
	import io.sarl.activeannotation.MyDelegate
	[:On]
	class MyClass implements SomeInterface {
		def function(param : String) : int {
			return provideDelegate(
				"function",
				#[typeof(String)],
				#[param]).function(param)
		}
	
		def provideDelegate(methodName : String, parameterTypes : Class<?>[], arguments : Object[]) : SomeInterface {
			return new MyDelegate
		}
	}
	[:Off]
[:End:]


## @NoEqualityTestFunctionsGeneration

The [:noeqtestannon:] annotation disables the generation the equality test functions, i.e. `equals()` and `hashCode()` from
the field declarations.

By default, the SARL compiler generates the equality test functions from the type's fields. In several cases, this automatic
behavior should be avoiding because the standard equality test that is provided by the Java run-time environment should be used.
In this case, [:noeqtestannon:] annotation may be used to mark a type or a field for being excluded of the equality test generation. 

The annotation may mark a type, as in the following example.
In this case, no equality test function is generated within the marked type and all its subtypes.

[:Success:]
	[:On]
	import [:noeqtestannonfqn](io.sarl.lang.annotation.NoEqualityTestFunctionsGeneration)
	[:noeqtestannon](@NoEqualityTestFunctionsGeneration)
	class MyClass {
	  var field1 : int
	  var field2 : String
	}
	[:Off]
[:End:]


The annotation may mark a specific field in order to exclude it from the equality test generation.
In the following example, the [:noeqtestfield2:] field is marked with the annotation. Consequently, it
is not included within the equality test within the `equals()` function, and the hash code replied
by the `hashCode()` function does not include the hash code of the [:noeqtestfield2:] field.

[:Success:]
	import io.sarl.lang.annotation.NoEqualityTestFunctionsGeneration
	[:On]
	class MyClass {
	  var field1 : int
	  [:noeqtestannon!]
	  var [:noeqtestfield2](field2) : String
	}
	[:Off]
[:End:]


## @ToString

The [:tostringannon:] annotation enables to generate the function that replies the string representation
of an object, a.k.a. as the [:tostringfct:] function in a Java program.
All non-static fields of the annotated class, and all of its superclasses are used for generating the [:tostringfct:] function.
This annotation can be applied to object-oriented types. The agent-oriented types cannot be annotated.

Let's a basic example:

[:Success:]
	[:On]
	import [:tostringannonfqn](org.eclipse.xtend.lib.annotations.ToString)
	[:tostringannon](@ToString)
	class MyClass {
	  var field1 : int
	  var field2 : String
	}
	[:Off]
[:End:]
[:Failure:]
	import org.eclipse.xtend.lib.annotations.ToString
	@ToString agent MyAgent {
	}
[:End:]


The previous code is equivalent to:

[:Success:]
	import org.eclipse.xtend.lib.annotations.ToString
	import org.eclipse.xtext.xbase.lib.util.ToStringBuilder
	[:On]
	class MyClass {
	  var field1 : int
	  var field2 : String
	
	  def [:tostringfct]$toString()$ : String {
	    var buffer = new ToStringBuilder(this)
	    buffer.add("field1", this.field1);
	    buffer.add("field2", this.field2);
	    return buffer.toString
	  }
	}
	[:Off]
[:End:]


For brevity there are options to the annotation to hide field names, skip fields with null values and print everything on one line.



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
