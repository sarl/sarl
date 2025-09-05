# Working Memory for Agents

[:Outline:]

An agent working memory serves as a fundamental component designed to enable autonomous entities to store, retrieve, and manipulate knowledge dynamically during their operational lifecycle. The primary goal of such a memory system is to provide agents with a localized, efficient, and context-aware repository for maintaining internal state, facilitating reasoning, and supporting adaptive behavior in response to environmental stimuli or internal objectives.

## General Principle of a Working Memory

One of the core objectives of an agent working memory is to support dynamic reasoning and reactivity. Agents operate in environments that are often dynamic, uncertain, or partially observable, necessitating a mechanism to retain and update information that reflects the most recent state of their surroundings or internal computations. This memory system acts as an intermediary between perception and action, allowing agents to integrate sensory inputs, maintain intermediate results, and track the progress of ongoing tasks. By doing so, it bridges the gap between the agent’s perceptual capabilities and its deliberative or reactive processes, ensuring that decisions are grounded in up-to-date and contextually relevant information.

A fundamental principle underlying the design of an agent working memory is structured accessibility. Knowledge stored within this memory must be retrievable with minimal computational overhead, as agents frequently require rapid access to critical data to respond to time-sensitive events. This principle is often realized through key-value associations, where knowledge is indexed by unique identifiers or [scoped names](./Naming.md#naming-for-data-with-scopes), enabling efficient lookup and modification. The use of scopes further enhances this structure by allowing agents to disambiguate between different instances of similarly named knowledge, such as distinguishing between "temperature" in an indoor versus outdoor context. Such organization not only optimizes retrieval but also supports modularity, enabling agents to manage multiple, potentially overlapping, knowledge sets without conflict.

The flexibility of knowledge representation is also a key consideration. An agent’s working memory must accommodate a diverse range of data types, from primitive values to complex objects, and support operations such as conditional updates, existence checks, and bulk removal. This adaptability allows the memory to serve a variety of use cases, including maintaining task-specific state, caching intermediate computations, or storing contextual information that influences decision-making. Additionally, the memory system should provide mechanisms to handle edge cases, such as the presence of null or undefined values, either by explicitly permitting them or enforcing policies that remove or replace them to maintain consistency.

Another essential principle is concurrency control, particularly in multi-threaded or distributed agent architectures of SARL. Since agents may operate concurrently or interact with shared resources, working memory must incorporate mechanisms to prevent inconsistencies arising from simultaneous access or modification. Synchronization primitives, such as locks or atomic operations, are commonly employed to ensure thread safety, thereby preserving the integrity of the stored knowledge.

### Generic Implementation vs. Hard-coded Implementation

The SARL framework provides two complementary approaches for managing an agent’s internal state: a generic **working memory API** (that is described in the rest of this manual page) and **direct access to local fields**.
These mechanisms offer developers flexibility in how agents store, retrieve, and manipulate knowledge, catering to both structured, dynamic data management and efficient, low-level state handling.

The working memory API is a high-level, capacity-based interface that allows agents to store and manage knowledge in a structured, key-value manner. This API abstracts the underlying storage mechanism, enabling agents to associate data with unique identifiers (e.g., `ScopedDataName`) and perform operations such as insertion, retrieval, conditional updates, and deletion.

In contrast, SARL also allows agents to define local fields, which are very close to the traditional object-oriented member variables that can be accessed and modified directly within the agent’s code. Local fields provide a lightweight and efficient way to store agent-specific state, offering the performance benefits of direct memory access. They are well-suited for simple, static, or frequently accessed data that does not require the overhead of the working memory API. 

While local fields lack the advanced features of the working memory — such as scoping or synchronization — they offer simplicity and speed, making them ideal for managing core agent attributes.

#### Concept of Scope

In SARL, the concept of **scope** refers to a mechanism for organizing and disambiguating knowledge or data within an agent’s memory by associating it with a specific context or namespace. Scope serves as a logical container that groups related pieces of information, enabling agents to manage multiple instances of similarly named knowledge without conflict.

Scopes are often implemented as part of a composite [:scopeddataname:], which combines a base name (e.g., "temperature") with a scope (e.g., "indoor" or "outdoor"). This allows the agent to store and retrieve knowledge precisely, even when the same name is reused across different contexts. According to the [naming convention for scoped names](./Naming.md#naming-for-data-with-scopes), it is possible to refer to a scoped data with a URI string. For example, `data:/indoor/temperature` and `data:/outdoor/temperature` represent two distinct knowledge entries, each associated with its own value.


## Integration with SARL Agents

For using an working memory in your agent, you have not to do more than using the [:workingmemorycapacity:] capacity:

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses [:workingmemorycapacity](WorkingMemory)
		
		on Event {
			setKnowledge(new [:scopeddataname](ScopedDataName)("a", "b", "name"), "a value")
		}
	}
[:End:]

By default, a working memory implemented with a dictionary is used. You could select your preferred working memory implementation by changing the skill related to the [:workingmemorycapacity:] capacity in the agent.


## Capacity for using a Working Memory

The [:workingmemorycapacity:] capacity in SARL enables agents to store, retrieve, and manipulate internal knowledge dynamically. It provides a structured, thread-safe, and context-aware memory system, allowing agents to maintain state, reason about their environment, and adapt their behavior based on stored information.

This capacity abstracts the underlying storage mechanism, offering a uniform API for knowledge management while supporting custom implementations (e.g., in-memory dictionaries, persistent databases, or distributed stores). By default, SARL provides a dictionary-based implementation (`DictionaryWorkingMemory`), but developers can replace it with domain-specific solutions just by setting up the corresponding SARL skill in the agent.

### Knowledge Storage

* `[:setKnowledgeFct!](id : [:scopeddataname!], value : Object) : Object` inserts or updates a knowledge entry. The `id` is the name of the scoped data (see [Naming convention](./Naming.md#naming-for-data-with-scopes)). The previous value associated with `id`, or `null` if none existed is returned by this function.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:setKnowledgeFct](setKnowledge)(new ScopedDataName("a", "b", "name"), "a value")
		}
	}
[:End:]


* `[:setKnowledgeIfAbsentFct!](id : [:scopeddataname!], value : Object)` inserts a knowledge entry **only if it does not already exist**.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:setKnowledgeIfAbsentFct](setKnowledgeIfAbsent)(new ScopedDataName("a", "b", "name"), "a value")
		}
	}
[:End:]


* `[:setKnowledgeIfPresentFct!](id : [:scopeddataname!], value : Object) : Object` updates a knowledge entry **only if it already exists**. The previous value, or `null` if the entry did not exist is returned by this function.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:setKnowledgeIfPresentFct](setKnowledgeIfPresent)(new ScopedDataName("a", "b", "name"), "a value")
		}
	}
[:End:]



### Knowledge Retrieval

* `[:getKnowledgeFct!](id : [:scopeddataname!], type : Class<T> = null) : T` retrieves the value associated with `id`. If the `type` is provided, the value from the working memory is casted to this type. Exception [:knowledgemissingexception:] is thrown if the entry does not exist.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.workingmemory.[:knowledgemissingexception](KnowledgeMissingException)
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			var objectValue : Object = [:getKnowledgeFct](getKnowledge)(new ScopedDataName("a", "b", "name"))

			var stringValue : Object = getKnowledge(new ScopedDataName("a", "b", "name"), typeof(String))
		}
	}
[:End:]


### Knowledge Existence and Metadata

* `[:isDefinedFct!](id : [:scopeddataname!]) : boolean` checks if a knowledge entry exists.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			if ([:isDefinedFct](isDefined)(new ScopedDataName("a", "b", "name"))) {
				info("The knowledge 'name' is defined in the working memory")
			} else {
				info("The knowledge 'name' is not defined in the working memory")
			}
		}
	}
[:End:]


* `[:getDefinedNamesFct!] : Stream<[:scopeddataname!]>` returns a stream of all stored [:scopeddataname:] identifiers.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			var definedData = [:getDefinedNamesFct](getDefinedNames)
			info(definedData.toArray)
		}
	}
[:End:]


* `[:getDefinedForNameFct!]](name : String) : Stream<[:scopeddataname!]>` returns a stream of all [:scopeddataname:] identifiers matching the given `name` (across all scopes).
* `[:getDefinedListForNameFct!]](name : String) : List<[:scopeddataname!]>` returns a list of all [:scopeddataname:] identifiers matching the given `name` (across all scopes).

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			var definedData = [:getDefinedForNameFct](getDefinedForName)("name")
			info(definedData.toArray)
		}

		on Event {
			var definedData = [:getDefinedListForNameFct](getDefinedListForName)("name")
			info(definedData.toArray)
		}
	}
[:End:]


* `[getMemorySizeFct!]] : long` returns the total number of stored knowledge entries.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			var size = [:getMemorySizeFct](getMemorySize)
			info(size)
		}
	}
[:End:]


* `[:isMemoryEmptyFct!] : boolean` checks if the working memory is empty.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			if ([:isMemoryEmptyFct](isMemoryEmpty)) {
				info("Working memory is empty")
			} else {
				info("Working memory is not empty")
			}
		}
	}
[:End:]


### Knowledge Removal

* `[:removeKnowledgeFct!](id : [:scopeddataname!]) : Object` removes a knowledge entry. The removed value is returned by this function.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:removeKnowledgeFct](removeKnowledge)(new ScopedDataName("a", "b", "name"))
		}
	}
[:End:]


* [:removeNullValuedKnowledgesFct:] removes all entries with `null` values.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:removeNullValuedKnowledgesFct](removeNullValuedKnowledges)
		}
	}
[:End:]


* [:clearMemoryFct:] removes *all* knowledge entries.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory
		
		on Event {
			[:clearMemoryFct](clearMemory)
		}
	}
[:End:]

### Thread Safety

* `[:getWorkingMemoryLock!] : Object` provides a lock object for synchronized blocks outside the control of the working memory's implementation itself. This function is useful when you want to use the streams provided by the working memory.

[:Success:]
	package io.sarl.docs.reference.workingmemory
	import io.sarl.api.core.Logging
	import io.sarl.api.workingmemory.WorkingMemory
	import io.sarl.api.naming.name.ScopedDataName
	import io.sarl.lang.core.Event
	[:On]
	agent A {
		uses WorkingMemory, Logging
		
		on Event {
			synchronized([:getWorkingMemoryLock](getWorkingMemoryLock)) {
				getDefinedForName("name").forEach [
					info(it)
				]
			}
		}
	}
[:End:]



[:Include:](../../includes/legal.inc)
