# Deprecated API and Major Changes

[:Outline:]

This page provides the major changes into the SARL API that needs to update your source code
(deprecated API, major changes, etc.).

This page is divided into two parts: the deprecated features from the SARL API, and the deprecated features from
the SARL Run-time Environment (SRE).

## Changes in the SARL API

### Since 0.12


<table>
<thead>
<tr><th>Deprecated Type or Module</th><th>Deprecated Feature</th><th>Replacement</th></tr>
</thead><tbody>


<tr><td>AbstractDocumentationMojo</td><td>getBootClassPath</td>
	<td>No replacement.
	</td></tr>



<tr><td>AbstractSARLLaunchConfiguration</td><td>getClasspath</td>
	<td>Replace `getClasspath(...)` by `getClasspathAndModulepath(...)`.
	</td></tr>



<tr><td>Address</td><td>getUUID</td>
	<td>Replace `getUUID` by `getID`.
	</td></tr>

[:Success:]
    package io.sarl.docs.faq.deprecation
    import java.util.UUID
    import io.sarl.lang.core.Address
    agent X {
        def test(a : Address) : UUID {
            a.getID
        }
    }
[:End:]


<tr><td>Module io.sarl.eclipse-slf4j</td><td>all</td>
	<td>No replacement.
	</td></tr>



<tr><td>OpenEventSpace</td><td>register</td>
	<td>Replace `register(listener, true)` by `registerWeakParticipant(listener)`, and `register(listener, false)` by `registerStrongParticipant(listener)`.
	</td></tr>

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.lang.core.EventListener
    import io.sarl.core.OpenEventSpace
    agent X {
        def weak(listener : EventListener, s : OpenEventSpace) : void {
            s.registerWeakParticipant(listener)
        }
        def strong(listener : EventListener, s : OpenEventSpace) : void {
            s.registerStrongParticipant(listener)
        }
    }
[:End:]



<tr><td>ReflectExtensions</td><td>getDefaultNameFormatter</td>
	<td>Replace `getDefaultNameFormatter` by `getDefaultMethodNameFormatter`.
	</td></tr>
<tr><td>ReflectExtensions</td><td>setDefaultNameFormatter</td>
	<td>Replace `setDefaultNameFormatter` by `setDefaultMethodNameFormatter`.
	</td></tr>



<tr><td>SarlBatchCompiler</td><td>getJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>
<tr><td>SarlBatchCompiler</td><td>setJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>



<tr><td>SarlcConfig</td><td>getJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>
<tr><td>SarlcConfig</td><td>setJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>



<tr><td>SARLClasspathProvider</td><td>getJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>
<tr><td>SARLClasspathProvider</td><td>setJavaBootClasspath</td>
	<td>No replacement.
	</td></tr>



<tr><td>ScriptExecutor</td><td>setBootClassPath</td>
	<td>No replacement.
	</td></tr>



<tr><td>SREBootstrap</td><td>setOffline</td>
	<td>No replacement.
	</td></tr>


</tbody></table>

### Since 0.11


<table>
<thead>
<tr><th>Deprecated Type</th><th>Deprecated Feature</th><th>Replacement</th></tr>
</thead><tbody>
<tr><td>DefaultContextInteractions</td><td>willReceive</td>
	<td>Replace `receiver.willReceive(new [:myeventname!])` by `emit(new [:myeventname!]) [it.ID == receiver]`.
	The number of functions in the API should be limited. This function is assumed to be redundant with `emit`.
	</td></tr>

[:Success:]
    package io.sarl.docs.faq.deprecation
    import java.util.UUID
    import io.sarl.core.DefaultContextInteractions
    event [:myeventname](MyEvent)
    agent X {
        uses DefaultContextInteractions
        def fct(receiver : UUID) : void {
            emit(new MyEvent) [it.ID == receiver]
        }
    }
[:End:]


<tr><td>Lifecycle</td><td>return type of `spawn`</td>
	<td>In SRE 2.x, we could spawn an agent and get their assigned UUID code via the spawn method
	the `Lifecycle` capacity, e.g.:
<pre><code>val agent_UUID = spawn(typeof([:myagentname!]))
</code></pre>
	Because of problems and inconsistencies caused by the parallel execution of the `spawn` function,
	the return value of this method has been deprecated. The `spawn` function replies nothing since version
	0.11 of the API.
	To spawn an agent and grab its UUID code, we first generate the identifier and then spawn the agent with it:
[:Success:]
    package io.sarl.docs.faq.deprecation
    import java.util.UUID
    import io.sarl.core.DefaultContextInteractions
    import io.sarl.core.Lifecycle
    agent [:myagentname](MyAgent) { }
    agent X {
        uses [:dcicapacity](DefaultContextInteractions), Lifecycle
        def fct(receiver : UUID) {
        	[:On]
            val agent_UUID = UUID::randomUUID
			spawnInContextWithID(typeof(MyAgent), agent_UUID, [:defaultContextCall](defaultContext))
			[:Off]
        }
    }
[:End:]
	([:defaultContextCall:] is the short for `getDefaultContext` provided by [:dcicapacity:])
	<br>
	or, with the version 0.12 of the API:
[:Success:]
    package io.sarl.docs.faq.deprecation
    import java.util.UUID
    import io.sarl.core.DefaultContextInteractions
    import io.sarl.core.Lifecycle
    agent [:myagentname](MyAgent) { }
    agent X {
        uses Lifecycle
        def fct : void {
        	[:On]
            val agent_UUID = UUID::randomUUID
			spawnWithID(typeof(MyAgent), agent_UUID)
			[:Off]
        }
    }
[:End:]
	</td></tr>


<tr><td>OpenEventSpace</td><td>register</td>
	<td>Replace `space.register(participant, isWeak)` by `[:registerstrongparticipantfct!](participant)`
	if `isWeak` is true, or `[:registerweakparticipantfct!](participant)` if `isWeak` is false. This change
	is applied by internal design on the space implementation.
	</td></tr>

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.core.OpenEventSpace
    import io.sarl.lang.core.EventListener
    agent X {
        def fct0(sp : OpenEventSpace, listener : EventListener) {
			sp.[:registerstrongparticipantfct](registerStrongParticipant)(listener)
        }
        def fct1(sp : OpenEventSpace, listener : EventListener) {
			sp.[:registerweakparticipantfct](registerWeakParticipant)(listener)
        }
    }
[:End:]

</tbody></table>


### Since 0.10


<table>
<thead>
<tr><th>Deprecated Type</th><th>Deprecated Feature</th><th>Removal Version</th><th>Replacement</th></tr>
</thead><tbody>
<tr><td>DefaultContextInteractions</td><td>spawn</td><td></td>
	<td>Replace `[:spawnfctname!](type, args)` by `[:lifecyclecapacity!].[:spawnfctname!](type, args)`.
	The definition of the [:spawnfctname:]into `DefaultContextInteractions` is not expected by SARL
	users since this capacity seems to be
	dedicated to interactions. [:lifecyclecapacity:] capacity is a better place for defining the `spawn`
	functions.

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.core.[:lifecyclecapacity](Lifecycle)
    agent MyAgent {}
    agent X {
    	uses Lifecycle
        def fct(args : Object[]) {
			typeof(MyAgent).[:spawnfctname](spawn)(args)
        }
    }
[:End:]

	</td></tr>
<tr><td>Logging</td><td>println</td><td></td>
	<td>Replace `println(msg)` by `[:infofctname!](msg)`. The semantic of `println` is linked to the system console.
	Agents are supposed to log their messages on the agent console.

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.core.[:loggingcapacity](Logging)
    agent X {
    	uses Logging
        def fct {
			[:infofctname](info)("msg")
        }
    }
[:End:]

	</td></tr>
<tr><td>SarlSpecificationChecker</td><td>getSarlSpecificationVersion</td><td></td>
	<td>Replaced by [:checkerfctname:]. Change due to the change of the return type
	that is now of type [:versiontypename:].

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.sarlspecification.[:checkername](SarlSpecificationChecker)
    import org.osgi.framework.Version
    agent X {
        def fct(s : SarlSpecificationChecker, t : Class<?>) : [:versiontypename](Version) {
			s.[:checkerfctname](getSarlSpecificationVersionObject)(t)
        }
    }
[:End:]

	</td/></tr>
</tbody></table>


### Since 0.5


<table>
<thead>
<tr><th>Deprecated Type</th><th>Deprecated Feature</th><th>Removal Version</th><th>Replacement</th></tr>
</thead><tbody>
<tr><td>DefaultContextInteractions</td><td>receive</td><td>0.10</td>
	<td>Replace `receiver.receive(new Event)` by `emit(new Event) [it.ID == receiver]`. The number of
	functions in the API should be limited. This function is assumed to be redundant with `emit`.
	</td></tr>
</tbody></table>


## Changes in the SARL Run-time Environment


## Since 0.10 (Janus 3.0.10.0)


<table>
<thead>
<tr><th>Deprecated Type</th><th>Deprecated Feature</th><th>Removal Version</th><th>Replacement</th></tr>
</thead><tbody>
<tr><td>Boot</td><td>setOffline(boolean)</td><td>0.10</td>
	<td>This parameter is not any more needed in the kernel of Janus v3. Indeed, the Janus v3 kernel is now
    based on a new plugin architecture (based on the <a href="https://bootique.io/">Bootique API</a>).
	With this plugin API, we could write a core software (the kernel) with a minimum set of features and
	that could be extended with plugins by only adding these plugins on the classpath of the application.
	In the context of Janus, the network features were moved into a plugin. One could easily add the
	network feature by adding the corresponding plugin jar file in the classpath (equivalent to the old
	`Boot.offline = false`) or remove it from the classpath (equivalent to `Boot.offline = true`), or
	even add another networking plugin that is based on another technology (e.g. MQTT, etc.).
	So, `Boot.offline` function is not any more into the Janus kernel (i.e. the Boot class, see below)
	but into the configuration of the networking plugin. This plugin is under validation and is planned
	to be back in SARL 0.12.
	</td></tr>
<tr><td>Boot</td><td>setVerboseLevel(level)</td><td>0.10</td>
	<td>This function is removed from `Boot`. The `SRE` utility class provides the replacing function.
	Note that it must be called before any launch of agent since the configuration level of Janus is set at start up.

[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.bootstrap.SRE
    agent X {
        def fct {
			SRE::getBootstrap.setVerboseLevel(1)
        }
    }
[:End:]

	</td></tr>
<tr><td>Boot</td><td>startJanus(type)</td><td>0.10</td>
	<td>In the old SRE 2.x, one would start an agent from Java by directly using the <code>Boot</code>
	class in Janus:
<pre><code>import io.janusproject.Boot
Boot.startJanus(typeof(MyAgent))
	</code></pre>
	In order to avoid any problems due to changes into the Janus implementation, in SRE 3.x it is preferable to use the SRE
	utility class as follows:
[:Success:]
    package io.sarl.docs.faq.deprecation
    import io.sarl.bootstrap.SRE
    agent MyAgent { }
    agent X {
        def fct {
        	[:On]
			var sre = SRE::bootstrap
			sre.startAgent(typeof(MyAgent))
			[:Off]
        }
    }
[:End:]
	</td></tr>
</tbody></table>


[:Include:](../legal.inc)
