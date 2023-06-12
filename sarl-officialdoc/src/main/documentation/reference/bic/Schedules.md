# Schedules Capacity

[:Outline:]

The built-in capacity `[:schedules](Schedules)` enables the agent to schedule tasks for future or periodic execution.

<!--- Test that all the documented functions are defined in the capacity, and no function is missed to be
      documented --> 
[:Fact:]{typeof(io.sarl.core.[:schedules!]).shouldHaveMethods(
	"[:task](task)(java.lang.String) : io.sarl.core.[:agenttask](AgentTask)",
	"[:setname](setName)(io.sarl.core.AgentTask, java.lang.String)",
	"[:execute](execute)(org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"execute(io.sarl.core.AgentTask, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"[:in](in)(io.sarl.core.AgentTask, long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"in(long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"[:every](every)(io.sarl.core.AgentTask, long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"every(long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"[:atfixeddelay](atFixedDelay)(io.sarl.core.AgentTask, long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"atFixedDelay(long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"[:at](at)(io.sarl.core.AgentTask, long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"at(long, org.eclipse.xtext.xbase.lib.Procedures$Procedure1) : io.sarl.core.AgentTask",
	"[:cancel](cancel)(io.sarl.core.AgentTask, boolean) : boolean",
	"cancel(io.sarl.core.AgentTask) : boolean",
	"[:iscanceled](isCanceled)(io.sarl.core.AgentTask) : boolean",
	"[:getactivetasks](getActiveTasks) : io.sarl.lang.util.ConcurrentSet")
}

## Creating Named Tasks

A named task may be created with:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	interface Tmp {
	[:On]
		def [:task!](name : String) : AgentTask
	[:Off]
	}
[:End:]


The replied task may be used for future execution, or controlling the execution.

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	[:On]
	agent A {
		uses Schedules
	
		var t : AgentTask
	
		def action {
			t = task("abc")
		}
	}
	[:Off]
[:End:]


## Changing the name of a task

A task has a name that serves as its identifier. You could change the task name by calling the following function:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	interface Tmp {
	[:On]
		def [:setname!](task : AgentTask, name : String)
	[:Off]
	}
[:End:]


Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Initialize
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	[:On]
	agent A {
		uses Schedules
	
		var t : AgentTask
	
		on Initialize {
			t = task("abc")
		}

		def action {
			this.t.[:setname!]("newName")
		}
		
	}
	[:Off]
[:End:]


## Launching a Task for a single run

For running a task once time, the following function is provided:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:execute!](task : AgentTask = null, procedure : (Agent) => void) : AgentTask
	[:Off]
	}
[:End:]


Without its optional argument, the function submits the given procedure (a lambda expression as defined in
the [General Syntax Reference](../GeneralSyntax.md)) to an executor provided by the runtime
platform. The execution of the procedure will be executed once time as soon as possible. This function
replies the agent task for controlling its execution.

With the optional argument, the function behaves in a similar way as previously, except that it accepts an
agent task as parameter. This task will attach to the given procedure. The replied task is the same as
the task given as parameter.

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Logging
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	[:On]
	agent A {
		uses Schedules, Logging
	
		var t1 : AgentTask
		var t2 : AgentTask
	
		def action {
			t1 = execute [ a : Agent |
				info(a)
			]
			t1 = t2.execute [ a : Agent |
				info(a)
			]
		}
	}
	[:Off]
[:End:]


## Launching a Delayed Task

For running a task in a given delay, the following function is provided:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:in!](task : AgentTask = null, delay : long, procedure : (Agent) => void) : AgentTask
	[:Off]
	}
[:End:]


Without its optional argument, the function submits the given procedure (a lambda expression as defined in
the [General Syntax Reference](../GeneralSyntax.md)) to an executor provided by the runtime
platform. The execution of the procedure will be delayed during the given number of milliseconds.
This function replies the agent task for controlling its execution.

With its optional argument, the function behaves in a similar way as the first, except that it
accepts an agent task as parameter. This task will attach to the given procedure. The replied task
is the same as the task given as parameter.


Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Logging
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	[:On]
	agent A {
		uses Schedules, Logging

		var t1 : AgentTask
		var t2 : AgentTask

		def myaction {
			t1 = in(1000) [ a : Agent |
				info(a)
			]
			
			t1 = t2.in(1000) [ a : Agent |
				info(a)
			]
		}
	}
	[:Off]
[:End:]


## Launching a Task at a Specific Time

For running a task at a specific time, the following function is provided:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:at!](task : AgentTask = null, time : long, procedure : (Agent) => void) : AgentTask
	[:Off]
	}
[:End:]


Without its optional argument, the function submits the given procedure (a lambda expression as defined in
the [General Syntax Reference](../GeneralSyntax.md)) to an executor provided by the runtime
platform. The execution of the procedure will start at the provided time.
If the given time is not in the futur, the task is not run.
This function replies the agent task for controlling its execution.

With its optional argument, the function behaves in a similar way as the first, except that it
accepts an agent task as parameter. This task will attach to the given procedure. The replied task
is the same as the task given as parameter.


Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Logging
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	[:On]
	agent A {
		uses Schedules, Logging

		var t1 : AgentTask
		var t2 : AgentTask

		def myaction {
			t1 = at(1000) [ a : Agent |
				info(a)
			]
			
			t1 = t2.at(1000) [ a : Agent |
				info(a)
			]
		}
	}
	[:Off]
[:End:]


## Launching a Periodic Task at a Fixed Rate

For running a periodic task with a fixed starting rate, the following function is provided:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:every!](task : AgentTask = null, delay : long, procedure : (Agent) => void) : AgentTask
	[:Off]
	}
[:End:]


The function without the default parameter submits the given procedure (a lambda expression as defined in
the [General Syntax Reference](../GeneralSyntax.md)) to
an executor provided by the runtime platform. The execution of the procedure
will be launched periodically with a period of the given number of milliseconds.
This function replies the agent task for controlling its execution.

The function with the default parameter behaves in a similar way as the first, except that it
accepts an agent task as parameter. This task will attach to the given
procedure. The replied task is the same as the task given as parameter.

If the duration of the task is greater to the given period length, then
multiple task's instances will be run in parallel.
For example, consider the following code:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	import static java.lang.Thread.*
	agent A {
		uses Schedules
		def myaction {
			[:On]
			[:every!](500) [ sleep(2000) ]
			[:Off]
		}
	}
[:End:]


At a given time, four instances of the task are run in parallel (A, B, C, D for example):


| t= | 0 | 500 | 1000 | 1500 | 2000 | 2500 | 3000 | 3500 | 4000 |
| -- | - | --- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| A  | X | X   | X    | X    |      |      |      |      |      |
| B  |   | X   | X    | X    | X    |      |      |      |      |
| C  |   |     | X    | X    | X    | X    |      |      |      |
| D  |   |     |      | X    | X    | X    | X    |      |      |
| E  |   |     |      |      | X    | X    | X    | X    |      |
| F  |   |     |      |      |      | X    | X    | X    | X    |



## Launching a Periodic Task with a Fixed Delay between the Runs

For running a periodic task with a fixed duration between the runs, the following function is provided:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:atfixeddelay!](task : AgentTask = null, delay : long, procedure : (Agent) => void) : AgentTask
	[:Off]
	}
[:End:]


Without its optional argument, the function submits the given procedure (a lambda expression as defined in
the [General Syntax Reference](../GeneralSyntax.md)) to an executor provided by the runtime
platform. The execution of the procedure will be launched periodically with a duration between the runs
of the given number of milliseconds. This function replies the agent task for controlling its execution.

With its optional argument, the function behaves in a similar way as the first, except that it
accepts an agent task as parameter. This task will attach to the given procedure. The replied task is
the same as the task given as parameter.

The [:atfixeddelay:] function has not the same issue ass the [:every:] function regarding the possibility
to have several runs in parallel.
The [:atfixeddelay:] function ensures that only one run of the procedure will be executed at a given time.

For example, the following code may be illustrated by the table below.

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	import static java.lang.Thread.*
	agent A {
		uses Schedules
		def myaction {
			[:On]
			[:atfixeddelay!](500) [ sleep(2000) ]
			[:Off]
		}
	}
[:End:]



| t= | 0 | 500 | 1000 | 1500 | 2000 | 2500 | 3000 | 3500 | 4000 | 4500 | 5000 | 5500 | 6000 | 6500 |
| -- | - | --- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- | ---- |
| A  | X | X   | X    | X    |      |      |      |      |      |      |      |      |      |      |
| B  |   |     |      |      |      | X    | X    | X    | X    |      |      |      |      |      |
| C  |   |     |      |      |      |      |      |      |      |      | X    | X    | X    | X    |



## Cancelling a Task

It may be useful to cancel a running task, e.g. a periodic task. The [:schedules:] capacity
provides the following functions for managing the execution cancellation of an agent task:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	interface Tmp {
	[:On]
		def [:cancel!](task : AgentTask, [:mayinterrupt](mayInterruptIfRunning) : boolean = true) : boolean
		def [:iscanceled!](task : AgentTask) : boolean
	[:Off]
	}
[:End:]


The first function will reply `false` if the task has already completed, has already been canceled,
or could not be canceled for some other reason (a failure means replying false).
If successful, and this task has not started when [:cancel:] is called, this task should never
run. If the task has already started, then the [:mayinterrupt:] parameter determines
whether the thread executing this task should be interrupted in an attempt to stop the task.

The [:iscanceled:] function enables to test if a task was canceled or not.

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	import io.sarl.core.AgentTask
	[:On]
	agent A {
		uses Schedules
		
		var t1 : AgentTask
		var t2 : AgentTask
		var t3 : AgentTask

		def myaction {
			t1 = in(1000) [ ]
			t2 = in(5000) [ ]
			t2 = in(10000) [ ]

			if (!t1.isCanceled) {
				t1.cancel
			}
			if (!t2.isCanceled) {
				t2.cancel(true)
			}
			if (!t3.isCanceled) {
				t3.cancel(false)
			}
		}
	}
[:End:]


## Conditional Execution of a Task

Sometimes, it may be useful to execute a task if a condition is `true` or `false`. The 
[:agenttask:] type, which is representing an instance of [:agenttask:] provides
the functions for assosiating a condition, named the guard, to the task:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:getguard!] : (Agent) => boolean
		def [:setguard!](condition : (Agent) => boolean)
	[:Off]
	}
[:End:]


[:Fact:]{typeof(io.sarl.core.AgentTask).shouldHaveMethod("[:getguard](getGuard) : org.eclipse.xtext.xbase.lib.Functions$Function1")}
[:Fact:]{typeof(io.sarl.core.AgentTask).shouldHaveMethod("[:setguard](setGuard)(org.eclipse.xtext.xbase.lib.Functions$Function1)")}


The first function replies the guard associated to the task, or `null` if
there is no associated guard. The second function enables you to change the associated guard.

Additionaly, the [:agenttask:] type provides utility functions for easier guard association:  

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.AgentTask
	import io.sarl.lang.core.Agent
	interface Tmp {
	[:On]
		def [:iftrue](ifTrue)(condition : (Agent) => boolean) : AgentTask
		def [:unless](unless)(condition : (Agent) => boolean) : AgentTask
	[:Off]
	}
[:End:]


[:Fact:]{typeof(io.sarl.core.AgentTask).shouldHaveMethod("[:iftrue!](org.eclipse.xtext.xbase.lib.Functions$Function1) : io.sarl.core.AgentTask")}
[:Fact:]{typeof(io.sarl.core.AgentTask).shouldHaveMethod("[:unless!](org.eclipse.xtext.xbase.lib.Functions$Function1) : io.sarl.core.AgentTask")}


The [:iftrue:] function is equivalent to [:setguard:], except that it is replying the current agent task.
The [:unless:] function sets the guard of the task to the negation of the given condition. It replies
the current task.

> **_Caution:_** The [:iftrue:] and [:unless:] functions should not be used on the result of the scheduling functions.
> Indeed, if you call these two function on the value replied by `execute` for example, the execution platform
> could have launched the task before the guard is set.

Consider the following code:
[:Success:]
 	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	agent A {
		uses Schedules
		def doSomething { }
		def myaction {
			var myVar : int
			[:On]
			[:execute!] [ doSomething ].[:unless!] [ myVar > 5 ]
			[:Off]
		}
	}
[:End:]

The call to [:execute:] is done before the call to [:unless:]. It means that the execution platform could have
already checked if a guard is assosiated and `true`, before the [:unless:] function sets the guard.

The best practice for setting the task guards is to create a task, set the guard, and execute the task:

[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	agent A {
		uses Schedules
		def doSomething { }
		def myaction {
			var myVar : int
			[:On]
			// Create the task instance
			var myTask = task(null)

			// Set the guard
			myTask.unless [ myVar > 5 ]

			// Execute the task
			myTask.execute [ doSomething ]
			[:Off]
		}
	}
[:End:]


## Retreiving the active tasks

The list of the active tasks may be retreived by invoking the following function:

[:Success:]
	package io.sarl.docs.reference.bic
	import java.util.Collection
	interface Tmp {
	[:On]
		def [:getactivetasks!] : Collection<String>
	[:Off]
	}
[:End:]


The replied collection is unmodifiable and contains the names of the active tasks. 

Example:
[:Success:]
	package io.sarl.docs.reference.bic
	import io.sarl.core.Schedules
	import io.sarl.core.Logging
	agent A {
		uses Schedules, Logging
		def myaction {
			[:On]
			for (taskName : [:getactivetasks!]) {
				info("Active task: " + taskName)
			}
			[:Off]
		}
	}
[:End:]


[:Include:](../../legal.inc)
