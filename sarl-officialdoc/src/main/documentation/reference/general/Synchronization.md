# Synchronization Expression

The synchonization expression does the same as it does in Java (see
[Java Language Specification](http://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.19)).
The only difference is that in SARL it is an expression and can therefore be used at more places. 

### Why Synchronization?

Let's consider a typical race condition where we calculate the sum, and multiple event handlers (i.e. threads) execute the
[:calculateevent:] function:

[:Success:]
	package io.sarl.docs.reference.runtime
	[:On]
	class Example {

	    var [:sumfield](sum) = 0

		def getSum : int {
			this.sum
		}

		def setSum(v : int) {
			this.sum = v
		}

		def [:calculateevent](calculate) : void {
	        setSum(getSum + 1)
		}
	}
[:End:] 


Since the [:calculateevent:] function may be executed in parallel, a multi-threading issues for accessing to the [:sumfield:] may occur.

Let the calling of `1000` times the [:calculateevent:] function. If we would execute this serially, the expected output would be `1000`,
but our multi-threaded execution fails almost every time with an inconsistent actual output e.g.: `965`. This result is of course
not unexpected.

A simple way to avoid the race condition is to make the operation thread-safe by using the [:sync:] keyword.


## The Synchronized Keyword

The [:sync:] keyword can be used on different levels:

* Instance methods
* Static methods
* Code blocks

When we use a [:sync:] block, internally SARL uses a monitor also known as monitor lock or intrinsic lock, to provide synchronization.
These monitors are bound to an object, thus all synchronized blocks of the same object can have only one thread executing them
at the same time.


### Synchronized Instance Methods

Simply add the [:sync:] keyword in the method declaration to make the method synchronized:

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
		def getSum : int {0}
		def setSum(v:int) {}
	[:On]
		[:sync](synchronized) def synchronizedCalculate : void {
		    setSum(getSum + 1)
		}
	[:Off]
	}
[:End:] 


Instance methods are synchronized over the instance of the type owning the method. Which means only one thread per
instance of the type can execute this method.


### Synchronized Static Methods

Static methods are synchronized just like instance methods:

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
	[:On]
		static var staticSum : int

		synchronized static def synchronizedCalculate : void {
		    staticSum = staticSum + 1
		}
	[:Off]
	}
[:End:] 


These methods are synchronized on the `Class` object associated with the type and since only one `Class` object exists per
virtual machine per type, only one thread can execute inside a static synchronized method per type, irrespective of the number
of instances it has.


### Synchronized Blocks Within Blocks

#### Standard Syntax

Sometimes we do not want to synchronize the entire method but only some instructions within it.
This can be achieved by applying [:sync:] to a block:

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
		def getSum : int {0}
		def setSum(v:int) {}
	[:On]
		synchronized def synchronizedCalculate : void {
			synchronized ([:syncparam](this)) {
			    setSum(getSum + 1)
			}
		}
	[:Off]
	}
[:End:] 


Notice that we passed a parameter [:syncparam:] to the synchronized block. This is the monitor object, the code inside the
block gets synchronized on the monitor object. Simply put, only one thread per monitor object can execute inside that
block of code.

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
		def getSum : int {0}
		def setSum(v:int) {}
	[:On]
		var lock = new Object
		synchronized def synchronizedCalculate : void {
			synchronized (lock) {
			    setSum(getSum + 1)
			}
		}
	[:Off]
	}
[:End:] 


In case the method is static, we would pass the class name in place of the object reference. And the class would be a
monitor for synchronization of the block:

[:Success:]
	package io.sarl.docs.reference.sync
	[:On]
	class Example {
		static var staticSum : int

		static def synchronizedCalculate : void {
			synchronized (typeof(Example)) {
			    staticSum = staticSum + 1
			}
		}
	}
[:End:] 


#### Expression Syntax

Because the [:sync:] keyword is an expression, it is possible to write synchronized code inside another expression.

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
		var sum : int
	[:On]
		var lock = new Object
		synchronized def synchronizedCalculate {
			var newValue = synchronized (lock) {
			    this.sum++
			}
		}
	[:Off]
	}
[:End:]


### Reentrancy of the Locks

The lock behind the synchronized methods and blocks is reentrant. That is, the current thread can acquire the same
synchronized lock over and over again while holding it:

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
	[:On]
		val lock = new Object
		def synchronizedCalculate : void {
			synchronized (lock) {
			    println("First time acquiring it");
			 
			    synchronized (lock) {
			        println("Entering again");
			 
			         synchronized (lock) {
			             println("And again");
			         }
			    }
			}
		}
	[:Off]
	}
[:End:] 


As shown above, while we're in a synchronized block, we can acquire the same monitor lock repeatedly.



## Synchronization of Fields

In a multi-threaded environment, a race condition occurs when two or more threads attempt to update mutable shared data
at the same time. SARL offers a mechanism to avoid race conditions by synchronizing thread access to shared data: a piece
of logic marked with [:sync:] becomes a synchronized block, allowing only one thread to execute at any given time.

Synchronizing on a field (within an agent, a behavior, or a skill) synchronizes not on the field itself, but on the object
assigned to it.
So synchronizing on a non-final field makes it possible for the field's value to change while a thread is in a block
synchronized on the old value. That would allow a second thread, synchronized on the new value, to enter the block at
the same time.

The story is very similar for synchronizing on parameters; two different threads running the method in parallel could
pass two different object instances in to the method as parameters, completely undermining the synchronization.

### Noncompliant Code Example

The following example shows a noncompliant usage of a lock.
When running the [:sync:] block, the lock is on object instance [:redcolorcst:] referred to by the [:colorfield:] variable.
When this variable is changed to [:greencolorcst:], other threads are allowed to enter into the synchronized block.
That may not be the expected behavior.

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
	[:On]
		var [:colorfield](color) = [:redcolorcst]("red")
		
		def doSomething : void {
			synchronized (color) {
				// ...
				color = [:greencolorcst]("green")
				// ...
			}
		}
	[:Off]
	}
[:End:] 


Another noncompliant usage of the synchronized blocks is:

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
	[:On]
		def doSomething : void {
			synchronized (new Object()) {
				// ...
			}
		}
	[:Off]
	}
[:End:]


In this case, a new lock object is created each time the synchronized block is reached. It means that the lock object is not
shared by the different threads. Consequently, there is no synchronization between the threads.


### Compliant Code Example

The following code shows a compliant usage of the lock object in order to avoid the issues that are explained in the previous section.
It it a better practice to create the lock object, named [:lockobject:], in order to be shared by the different threads.
The best place is to put it as a field of the enclosing type (as illustrated below). 

[:Success:]
	package io.sarl.docs.reference.sync
	class Example {
	[:On]
		var color = "red"

		val [:lockobject](lockObj) = new Object
		
		def doSomething : void {
			synchronized (lockObj) {
				//...
				color = "green"
				// ...
			}
		}
	[:Off]
	}
[:End:]



[:Include:](../generalsyntaxref.inc)

[:Include:](../../legal.inc)
