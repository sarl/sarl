# Creating an extension for the Janus SRE

[:Outline:]

This document describes the basics of the extension of the Janus run-time environment (SRE).
Before reading this document, it is recommended reading
the [General Syntax Reference](../reference/GeneralSyntax.md).

> **_Caution:_** This tutorial will use the syntax of the SARL programming language for illustration. However, you could define your extension with any programming language that is Java ir compatible with Java, e.g. [Scala](http://www.scala-lang.org/) or [Kotlin](https://kotlinlang.org/).

## Janus Framework

[:Include:](../tools/janus.inc)

Before starting to create an extension for Janus, you have to be sure you have understood these key implementation choices.

## Bootique

[Bootique](http://bootique.io) is a platform for building container-less runnable Java applications.
It is designed for microservices (but not limited to), as it allows you to create a fully-functional application with minimal-to-no setup.
Unlike traditional container-based applications, Bootique allows you to control your `main()` method and create Java applications that behave like simple executable commands.
Each Bootique application can be started with a (YAML) configuration, or configured with shell
variables and is ideally suited for Docker and cloud deployments.

Compared to other products, Bootique has a focus on modularity and clean pluggable architecture.
It is built on top of a [dependency injection container](https://en.wikipedia.org/wiki/Dependency_injection)
(but not [Google Guice](https://github.com/google/guice) that is another injector used by the SARL compiler itself),
and pretty much anything in Bootique can be customized/overridden.

*Why is Bootique used into Janus?*
As described above, the features of bootique enables the define and *override* injected modules with less effort.
Moreover, the Bootique enables to define complex (almost complete) configuration parameters that could be
changed from a YAML file, Java properties, environment variables and command-line options.
Finally, Bootique helps to define specific commands that could be started from the command-line
For all these reasons, Bootique was included into Janus. 

## Creating an extension step-by-step

This section explains how to create an extension to Janus step-by-step.
In order to create this extension, the following steps should be followed.

### Select the modules from Janus

For creating a Janus extension, even more complex than the one explained in this tutorial, you should study and understand
the implementation of Janus in order to select the best module or service to override for your purpose.

For matter of simplicity in this tutorial, the logging service of Janus is chosen for illustrating the extension creation.
The type that is defined in Janus for representing the logging service is [:loggingtype:].


### Write the extension code

After selecting the modules to be extended, you should write your code:

[:Success:]
	[:On]
	package mypackage
	import org.arakhne.afc.services.IService
	import io.sarl.sre.janus.services.logging.[:loggingtype](LoggingService)
	[:Off]
	import java.util.logging.Logger
	import java.util.logging.Level
	import org.arakhne.afc.services.AbstractService
	class [:myloggertype](MyLogger) extends Logger {
		new (name : String=null, parent : Logger=null) {
			super("", "")
		}
	}
	[:On]
	class [:myloggerservicetype](MyLoggingService) extends AbstractService implements LoggingService {

		val platformLogger = new MyLogger

		var kernelLogger : MyLogger

		override getReferenceType : Class<? extends IService> {
			typeof(LoggingService)
		}

		override [:getPlatformLogger](getPlatformLogger) : Logger {
			this.platformLogger
		}

		override [:getKernelLogger](getKernelLogger) : Logger {
			if (this.kernelLogger === null) {
				this.kernelLogger = new MyLogger(this.platformLogger)
			}
			return this.kernelLogger
		}

		override [:getKernelModuleLogger](getKernelModuleLogger)(moduleName : String) : Logger {
			return new MyLogger(moduleName, getKernelLogger)
		}

		override [:createAgentLogger](createAgentLogger)(name : String, initialLevel : Level) : Logger {
			val log = new MyLogger(name, this.platformLogger)
			if (initialLevel !== null) {
				log.level = initialLevel
			}
			return log
		}
		
		override [:onstartfunction](onStart) {
		}

		override [:onstopfunction](onStop) {
		}
		
	}
[:End:]

In the previous code, the type [:myloggertype:] is defined as an implementation of the JUL logger.
The construction takes as optional argument the name of the logger, and as mandatory argument the parent logger.

Four functions must be defined in your own implementation of the logging system, namely [:myloggerservicetype:]:

* [:getPlatformLogger:]: replies the root logger of all the loggers of the SRE.
* [:getKernelLogger:]: replies the logger that is dedicated to the SRE kernel.
* [:getKernelModuleLogger:]: replies the logger that is dedicated to a module of the SRE kernel. The argument of the function is the kernel module's name to be used when displaying the messages.
* [:createAgentLogger:]: create the logger for the agent with the given name. The second parameter permits to control the verbosity of the logger.


> **_Note:_** In the previous code, the interface [:loggingtype:] is directly implemented. It is also possible
> to extend a class that is defined into the Janus code and implementing this interface in order to have
> benefit of existing code.

The functions [:onstartfunction:] and [:onstopfunction:] are invoked when the service is started an stopped, respectively.
You could code in these two functions any intialization or destruction code.


### Determine the injection module

According to the architecture of Janus, the modules are injected into the Java objects that depend on these modules.

It is then mandatory to determine:

1. where the injection is defined, i.e. in which injection module, and
2. how the injection is defined, i.e. the binded types.

In Janus, the modules are defined into the package [:janusinjectionpackage:], or one of its sub-packages.
The module that corresponds to the logging system is defined in the class `[:loggingijnjectionpackage!].[:janusloggingservice!]`.
The definition of this module is close to:

[:Success:]
	[:On]
	package [:loggingijnjectionpackage]{[:janusinjectionpackage](io.sarl.sre.janus.boot).internal.services}
	import io.bootique.di.Binder
	import io.bootique.di.BQModule
	import io.sarl.sre.janus.services.logging.LoggingService
	[:Off]
	abstract class [:janusloggingservice](JulLoggingService) implements LoggingService {
	}
	[:On]
	class LoggingServiceModule implements BQModule {
		override configure(extension binder : Binder) {
			typeof(LoggingService).bind.to(typeof(JulLoggingService)).[:assingletonfct](inSingletonScope)
		}
	}
[:End:]

The key principle of the injection definition is the type binding: each time a type `A` needs to be injected, the injector creates an instance of the binded type `B`. This binding is defined with a code similar to `A.bind.to(B)`.
In the previous code, the type [:loggingtype:] that should be injected is binded to an instance of type [:janusloggingservice:], that is a Janus-internal definition of the logging service.
The function [:assingletonfct:] forces the injection engine to create the single instance, and no more.


### Write a module for injecting your new logging service

Now you have determined the original injection module and definition, you could define your own injection module for your extension.
The following code is the definition of the injection module that binds the [:loggingtype:] to the new implementation of the logging service.

[:Success:]
	[:On]
	package mypackage
	import io.bootique.di.Binder
	import io.bootique.di.BQModule
	import io.sarl.sre.janus.services.logging.LoggingService
	[:Off]
	abstract class MyLoggingService implements LoggingService {
	}
	[:On]
	class MyLoggingServiceModule implements BQModule {
		override configure(extension binder : Binder) {
			typeof(LoggingService).bind.to(typeof(MyLoggingService)).inSingletonScope
		}
	}
[:End:]


### Write a Bootique module provider

As explained previously, the Bootique framework is used for enabling the overriding of the injection definitions.
The API of this framework imposes to define a specific provider for the module in your extension. This provider has the role to:

1. Define name and the documentation of the extension.
2. Create the configuration binding, if any.
3. Define the overriding of the module, i.e. the existing injection module that is overriding by your own module.
4. Create the associated injection module on demand.

This provider has a central role into the Bootique architecture. Without this provider, it is impossible to
include an extension into your application without re-compiling all the source code of Janus with your extension
inside. This particular feature of bootique enables to load your extension at run-time without the need to
re-compile the Janus code itself.

Therefore, the code of the Bootique provider is:

[:Success:]
	[:On]
	package mypackage
	import io.sarl.sre.janus.services.logging.LoggingService
	import io.bootique.BQModuleMetadata
	import io.bootique.BQModuleProvider
	import io.bootique.di.Binder
	import io.bootique.di.BQModule
	import java.util.Collection
	[:Off]
	class MyLoggingServiceModule implements BQModule {
		def configure(binder : Binder) {}
	}
	abstract class LoggingServiceModule implements BQModule {
	}
	[:On]
	class MyLoggingServiceModuleProvider implements BQModuleProvider {

		override [:moduleBuilderfct](moduleBuilder) : BQModuleMetadata.Builder {
			BQModuleMetadata::[:factorybuilderfct](builder)(module)
				.[:providerNamefct](providerName)([:namefct](name))
				.description("This is my extension")
				.overrides([:overridesfct](overrides))
		}

		override [:modulefct](module) : BQModule {
			new MyLoggingServiceModule
		}

		override [:overridesfct](overrides) : Collection<Class<? extends BQModule>> {
			#[LoggingServiceModule]
		}

	}
[:End:]

The function [:moduleBuilderfct:] is the first of the mandatory functions to be defined into the provider.
It replies a factory for the Bootique module. This factory is built with the function [:factorybuilderfct:]
that takes the injection module instance (replied by the second function to be defined).
Then, it is possible to specify more attributes for the Bootique module, such as:

* The name of the provider with the function [:providerNamefct:] that takes the name of the module as argument. The function [:namefct:] replies this name. It is already defined into the super type.
* A description for the extension that is shown up in the help page of the application.
* The specification of the overridden modules. The function [:overridesfct:] replies the list of the injection module typenames that are overridden by your extension. By default, this function replies an empty list.

The function [:modulefct:] must be defined for creating the instance of the injection module.

The function [:overridesfct:] must reply the list of the module types that are overridden by the extension, here the original
logging module from Janus.

To conclude this section, the builder is used by the Bootique framework to create and include your extension into the application.


### Define the Bootique module provider as Java service

Defining the Bootique injection module provider, as in the previous section, is not enough to enable Bootique to use your extension.
Indeed, the Bootique must find your library dynamically (at run-time) on the application classpath.
Java framework defines a standard for discovering new features on the classpath: the Java services.

> **_Caution:_** the Java services are different than the SRE services. The first services are defined by JAva Consortium
> in order to extend your application dynamically (similar to plugins). The second services are designed to run the agents
> in the Janus framework.

It is almost easy to provide your Bootique module provider to the Bootique framework.
Indeed a Bootique module provider is assimilated to a Java service, and could be declared according to this Java standard.

[:Fact:](io.sarl.lang.core.SARLVersion::MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH == "17")

There is two methods for declared a Java service: one for Java 8 to 10, and one for Java 11 or higher.
Even if SARL requires the version [:Dynamic:](io.sarl.lang.core.SARLVersion::MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH) of the Java Virtual Machine, The two methods are explained because
they are still usable in Java [:Dynamic:](io.sarl.lang.core.SARLVersion::MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH) applications.

**For Java 8 or higher:** You must create a file into the folder `META-INF/services` with the name `io.bootique.BQModuleProvider` (that is the fully
qualified name of the Bootique module provider class). Each line of this file contains the fully qualified name of an implementation of
a Bootique module provider to be added to the application classpath. Therefore, you could add your own Bootique module provider in this file.
The content of the file `META-INF/services/io.bootique.BQModuleProvider` becomes:

```
mypackage.MyLoggingServiceModuleProvider
```


**For Java 11 or higher:** Since the introduction of the modules into the Java standards, you must declare the provided Java services
in the file `module-info.java`:

```java
module mypackage.myextension {
  requires bootique;
  provides io.bootique.BQModuleProvider with mypackage.MyLoggingServiceModuleProvider;
}
```


### Add the library in the classpath

Once you have written and compiled your extension to obtain the library file `myextension.jar`.
For including your extension into the application, you have only to add the jar file in the classpath (or the module path)
of your application.

For example, running the Janus framework with your extension may be done with the following command line:
```
janus -cp myextension.jar myagent
```


## Adding configuration

One of the interesting features of Bootique is its ability to define and manage configuration attributes.


### Bootique configuration in short

Bootique modules obtain their configuration in a form of "factory objects". The Bootique application configuration is multi-layered
and roughly follows the sequence of "code - config files (contributed) - config files (CLI) - overrides".
"Code" is the default values that are provided in constructors of factory objects.
Config files overlay those defaults with their own values. Config files can be either contributed in the code,
or specified on the command line. Files is where the bulk of configuration usually stored. Finally config values may be
further overridden via Java properties and/or environment variables.

Format of configuration file can be either JSON or YAML. For simplicity we'll focus on YAML format, but the two are
interchangeable. Here is an example config file:

```yaml
log:
  level: warn
  appenders:
    - type: file
      logFormat: '%c{20}: %m%n'
      file: target/logback/debug.log

jetty:
  context: /myapp
  connectors:
    - port: 12009
```

While not strictly required, as a rule the top-level keys in the file belong to configuration objects of individual modules.
In the example above `log` subtree configures bootique logging module, while `jetty` subtree configures bootique jetty module.


### Create a configuration object

Bootique allows each module to read its specific configuration subtree as an object of the type defined in the module code.
Very often such an object is written with a bunch of setters for configuration properties, and a factory method to produce
create the root instance of the configuration tree.
Here is an example configuration object:

[:Success:]
	[:On]
	package mypackage
	[:Off]
	import org.eclipse.xtend.lib.annotations.Accessors
	import io.bootique.annotation.BQConfigProperty
	import io.bootique.config.ConfigurationFactory
	[:On]
	class MyConfiguration {

		/** 
		 * Prefix for the configuration entries of the modules.
		 */
		public static val [:prefixstr](PREFIX) = "myextension"

		/** 
		 * Name of a configuration property (with prefix).
		 */
		public static val [:prop1name](PROP1_NAME) = PREFIX + ".prop1"

		/** 
		 * Default value for the property.
		 */
		public static val [:prop1defaultvalue](DEFAULT_PROP1_VALUE) = "default-value"

		@[:accessorsannotation](Accessors)(PUBLIC_GETTER)
		var [:prop1attr](prop1) : String = DEFAULT_PROP1_VALUE

		/** Change the value of prop1.
		 */
		@[:bootiquehelp](BQConfigProperty)("Explanation on the meaning of prop1.")
		def [:prop1setter](setProp1)(value : String) {
			this.prop1 = value
		}

		/** Replies the configuration tree.
		 */
		static def getConfiguration(configFactory : [:configfactorytype](ConfigurationFactory)) : MyConfiguration {
			configFactory.[:bootiquefactoryfct](config)(typeof(MyConfiguration), PREFIX)
		}

	}
[:End:]

The previous code provides the best pratices for written a Bootique-based configuration.

Each property of the configuration has a qualified name, which starts with the prefix of the configuration.
That's why a global variable named [:prefixstr:] is defined for this prefix.

For each property in the configuration, you must:

1. Choose the type of the property, e.g. `String`.
2. Define the qualitied name of the property,as illustrated by the declaration of the global variable [:prop1name:].
3. Define a default value for the property, as illustrated by the declaration of [:prop1defaultvalue:].
4. Define the property attribute, e.g. [:prop1attr:], with the selected property type, and the assignment of the default value. In the example, the active annotation [:accessorsannotation:] generates the public getter function for the attribute.
5. Define the setter function [:prop1setter:] that is in charge of changing the value of the property.
6. Annotate the function [:prop1setter:] with the annotation [:bootiquehelp:] that contains a text to be shown up in the help page of the application.


Finally, you must define a static factory function for creating the configuration object instance from the Bootique tool of type [:configfactorytype:].
The code of this factory function invokes the function [:bootiquefactoryfct:] that takes the type of the configuration to be created and the associated
property prefix. This function replies the instance of the configuration, already filled with the values read from the configuration source (file,
property, environment variable, command-line option).

> **_Note:_** Each of the property in a configuration is automatically mapped to a Java property with the name `bq.<fqn>`, where `<fqn>` is
> the fully qualified name of the property, e.g. `bq.myextension.prop1`.
> Therefore, it is possible to change the value of the property by specifying the property, e.g. on the command-line with `-Dbq.myextension.prop1="value"`.


### Injecting a configuration on demand

Now, it is important to define the injection of the extension's configuration.
A simple approach is to update the injection module by adding the function XXX.

[:Success:]
	[:On]
	package mypackage
	import io.sarl.sre.janus.services.logging.LoggingService
	import io.bootique.config.ConfigurationFactory
	import io.bootique.di.Binder
	import io.bootique.di.BQModule
	import io.bootique.di.Injector
	import io.bootique.di.Provides
	import javax.inject.Singleton
	[:Off]
	abstract class MyLoggingService implements LoggingService {
	}
	class MyConfiguration {
		static def getConfiguration(configFactory : ConfigurationFactory) : MyConfiguration {
		}
	}
	[:On]
	class MyLoggingServiceModule implements BQModule {
		override configure(extension binder : Binder) {
			typeof(LoggingService).bind.to(typeof(MyLoggingService)).inSingletonScope
		}
		@Provides
		@Singleton
		def [:provideConfig](provideConfig)(configFactory : ConfigurationFactory, injector : Injector) : MyConfiguration  {
			val config = MyConfiguration::getConfiguration(configFactory)
			injector.injectMembers(config)
			return config
		}
	}
[:End:]

The function [:provideConfig:] is added into the configuration module in order to create the configuration instance when
it should be injected.



## Mapping a configuration attribute to an environment variable

As explained previously, each configuration property is automatically mapped to a Java property.
However, it may be interesting to map the configuration property to an environment variable.

Since, it is a little complicated to use the Bootique API, we propose to use the class [:variabledeclstype:]
that provides tools for binding the bootique property to its environment variable.
The name of the environment variable is based on the name of the property, but upper-cased and by replacing
all the not alphanumeric characters by the underscore character.

The use of the class [:variabledeclstype:] must be done into the injection module:

[:Success:]
	[:On]
	package mypackage
	import io.sarl.sre.janus.services.logging.LoggingService
	import io.bootique.config.ConfigurationFactory
	import io.bootique.di.BQModule
	import io.bootique.di.Binder
	import io.bootique.di.Injector
	import io.bootique.di.Provides
	import org.arakhne.afc.bootique.variables.VariableDecls
	import javax.inject.Singleton
	[:Off]
	abstract class MyLoggingService implements LoggingService {
	}
	class MyConfiguration {
		public static val PROP1_NAME = ""
		static def getConfiguration(configFactory : ConfigurationFactory) : MyConfiguration {
		}
	}
	[:On]
	class MyLoggingServiceModule implements BQModule {
		override configure(extension binder : Binder) {
			typeof(LoggingService).bind.to(typeof(MyLoggingService)).inSingletonScope

			[:variabledeclstype](VariableDecls)::extend(binder).declareVar(MyConfiguration::PROP1_NAME)
		}
		@Provides
		@Singleton
		def [:provideConfig](provideConfig)(configFactory : ConfigurationFactory, injector : Injector) : MyConfiguration  {
			val config = MyConfiguration::getConfiguration(configFactory)
			injector.injectMembers(config)
			return config
		}
	}
[:End:]


## Mapping a configuration attribute to a command-line option

As for the environment variables, it is possible to map a configuration attribute to a command-line option.
This mapping is declared into the injection module:

[:Success:]
	[:On]
	package mypackage
	import io.sarl.sre.janus.services.logging.LoggingService
	import io.bootique.config.ConfigurationFactory
	import io.bootique.di.BQModule
	import io.bootique.di.Binder
	import io.bootique.di.Injector
	import io.bootique.di.Provides
	import io.bootique.meta.application.OptionMetadata
	import org.arakhne.afc.bootique.variables.VariableDecls
	import javax.inject.Singleton
	import static extension io.bootique.BQCoreModule.extend
	[:Off]
	abstract class MyLoggingService implements LoggingService {
	}
	class MyConfiguration {
		public static val PROP1_NAME = ""
		static def getConfiguration(configFactory : ConfigurationFactory) : MyConfiguration {
		}
	}
	[:On]
	class MyLoggingServiceModule implements BQModule {
		override configure(extension binder : Binder) {
			typeof(LoggingService).bind.to(typeof(MyLoggingService)).inSingletonScope

			VariableDecls::extend(binder).declareVar(MyConfiguration::PROP1_NAME)

			binder.extend.[:addoptionfct](addOption)([:optionmetadatabuilder](OptionMetadata::builder)("[:commandlineoptname](opt)", "This is the option to set prop1")
					.valueRequired("[:commandlineoptvalue](value)")
					.build)
					.mapConfigPath("opt", MyConfiguration::PROP1_NAME)
		}
		@Provides
		@Singleton
		def [:provideConfig](provideConfig)(configFactory : ConfigurationFactory, injector : Injector) : MyConfiguration  {
			val config = MyConfiguration::getConfiguration(configFactory)
			injector.injectMembers(config)
			return config
		}
	}
[:End:]

The Bootique API provides tools for declaring the command-line option, e.g. `--[:commandlineoptname!]`.
In the previous example, this command-line option added with [:addoptionfct:] takes a mandatory value, that is named [:commandlineoptvalue:] in the help pages of the application.
The function [:optionmetadatabuilder:] enables to create a description of the command-line option, with its name, descripton and its value description.


[:Include:](../legal.inc)
