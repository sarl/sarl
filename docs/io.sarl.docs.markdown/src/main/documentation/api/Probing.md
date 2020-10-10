# SRE Observing and Probes

[:Outline:]

The multi-agent system is composed of multiple elements, including agents, behaviors, contexts and spaces, but not limited to.
Organizational models in multi-agent systems usually position agents as actors-observers within environments shared
by multiple agents and organizational structures at different levels of granularity.
Then, the agents may have the capacity to observe the environment and the other agents, without directly interacting
with them. The observing agent is able to extract information from another agent or from a society of agents, that could be opaque
according to a possible holonic architecture of the system.

Observation means that data or information could be extracted from the observed object without filtering from this latter.
The observer is not intrusive from the agent point of view. In other words, the observed agent has no knowledge about the fact it
is observed or not. In this case, it cannot adapt its behavior on its observation status.

Because an agent is defined as an autonomous entity, the agent is supposed to be enabled to specify if a part of itself
is observable or not, i.e. to specify the access rights.

> **_Caution:_** The right access management is not yet supported by the SARL API.

## What is Observable?

The first question arising when considering the concept of observable multi-agent system is: what is observable?

Any object within the multi-agent system that could be referred with a name is possibly subject of an observation.
The objects are referred according to a [specific naming convention](./Naming.md).

Because observation is related to data extraction, only fields could be observed at the concrete level.
Consequently, the observable objects a the fields declared within an:
* agent
* behavior
* skill
* agent context
* space
* service
* artifact (if the SRE supports this concept)

## Probe: generic observer implementation

### General Principles

A probe is an implementation of the proxy and observer software design patterns for observable multi-agent system.
It is a software tool that extracts the data from the observed object and provide the data to the observer.
Since a probe is a proxy, it filters the interaction from the observer to the observed object by enabling the first
only to get the data (no other function declared into the observed object is accessible).
The implementation of a probe ensures that it is not intrusive to the observed object. 

The observer has to create a probe on a field of the observable object (agent, behavior, etc.).
Then, the probe could be read to obtain the data, or the observer could be notified each time the data has changed
(according to the observer software design pattern).

### Concrete Definition

A probe is defined as:

[:ShowType:](io.sarl.api.probing.Probe)

The functions are:
* `getName`: Reply the name of the probe, that is constant.
* `getUri`: Reply the URI of the observed object.
* `getValue` : Reply the observed value.
* `getType` : Reply the type of the observed value.
* `sync`: Force the synchronization of the observed value.
* `release`: Release any resource associated to the probe.
* `isActive`: Reply if this probe is active. When a probe is active, it could be synchronized to the observed object.
* `isInvalid`: Reply if this probe is invalid. When a probe is invalid, the value replied by `getValue` may not corresponds to the observed element's value.

The following function is provided for convenience, but it should be used with caution:
* `setValue`: Force the observed field to have a specific value (brokes the agent's autonomy)

### Observe the Probe

The observer software design pattern that enables to be notified when the observed value changed in implemented into the probe.
According to the standard implementation best practices of the Java programming language, an observer (in this design pattern)
is named a listener, and it is defined by an interface. Two types of observers are defined on probes: [:probelistener:]
and [:probereleaselistener:].

[:probelistener:] is defined as:

[:ShowType:](io.sarl.api.probing.[:probelistener]$IProbeListener$)


It corresponds to the observer on value changes.


[:probereleaselistener:] is defined as:

[:ShowType:](io.sarl.api.probing.[:probereleaselistener]$IProbeReleaseListener$)


It corresponds to the observer on the release of a probe.


The functions `addProbeListener`, `addProbeReleaseListener`, `removeProbeListener` and `removeProbeReleaseListener` enable to (un)register an event listener on the probe.


## Probing Service 

In order to manage and run the different probes, the SRE must implement a dedicated service: the [:probeservicename:].
It is defined as:

[:ShowType:](io.sarl.api.probing.[:probeservicename]$ProbeService$)



Creating a probe is done by calling the `probe` function. Basically, you need to specify the [name](./Naming.md) of the
observed field, the expected type of the value, and optionally the name of the probe. 

Through the probe service, you could force the synchronization of all the managed probes by calling the `sync` function.
This function forces all the probes to write the new values into the observed fields and to read the lastest values
of these fields.

Finally, two functions are provided:
* `getProbes` for retrieving the list of the managed probes
* `releaseAllProbes` releasing all the probes such that they become inactive.

To use the probe service, you have to get it from the SRE, as illustrated below:

[:Success:]
	package io.sarl.docs.namespace
	import io.sarl.bootstrap.SRE
	import io.sarl.api.probing.ProbeService
	class MyProgram {
		static def main(arguments : String*) {
			[:On]
			var bootstrap = SRE::getBootstrap
			var probeService = bootstrap.getService(typeof(ProbeService))
			[:Off]			
		}
	}
[:End:]


Then, an probe is attached to the field named [:emergencyfield:] of type [:fieldtype:] that is defined within the agent [:agentid:].
The name [:probename:] is given to the probe. 
The example loops for displaying the observed value (of course it is not the most efficient usage of a probe).

[:Success:]
	package io.sarl.docs.namespace
	import io.sarl.bootstrap.SRE
	import io.sarl.api.probing.ProbeService
	class MyProgram {
		static def main(arguments : String*) {
			var bootstrap = SRE::getBootstrap
			var probeService = bootstrap.getService(typeof(ProbeService))
			[:On]
			var probe = probeService.probe("agent:[:agentid]$a7fbd4cc-9e1a-48c3-8ee8-3a7974ccb05c$#[:emergencyfield]$emergency$", typeof([:fieldtype]$Integer$), "[:probename]$My Probe$")
			[:Off]			
		}
	}
[:End:]


Then, you could use the value that is extracted by the probe. The example below loops for displaying the observed
value (of course it is not the most efficient usage of a probe).

[:Success:]
	package io.sarl.docs.namespace
	import io.sarl.bootstrap.SRE
	import io.sarl.api.probing.ProbeService
	class MyProgram {
		static def main(arguments : String*) {
			var bootstrap = SRE::getBootstrap
			var probeService = bootstrap.getService(typeof(ProbeService))
			var probe = probeService.probe("", typeof(Integer), "")
			[:On]
			while (true) {
				println("Probe: " + probe.value)
			}
			[:Off]			
		}
	}
[:End:]


[:Include:](../legal.inc)
