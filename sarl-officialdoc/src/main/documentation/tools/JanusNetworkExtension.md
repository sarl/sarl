# Extending the Janus SRE with Network Connection

[:Outline:]

[:Include:](../includes/janus.inc)

By default, Janus does not provide a support for exchanging events over a computer
network. For enabling this communication feature, a specific Janus extension is needed.

This documentation page describes the extension [:networkextensionname:] that adds the communication
among Janus nodes over a computer network.

The extension described on this page was defined according to the [standard method for extending Janus](../tutorials/runtime/SreExtension.md).

## General Principles of the Extension

[:Include:](../includes/hazelcast.inc)

## Maven Inclusion

In order to include the networking extension into your project, you could add the following Maven dependency.

```xml
 <project>
   ...
    <properties>
       ...
       <janus.version>[:janus.version!]</janus.version>
    </properties>
    ...
    <dependencies>
       ...
       <dependency>
          <groupId>[:networkextensiongroup](io.sarl.sre.janus)</groupId>
          <artifactId>[:networkextensionname](janus.network)</artifactId>
          <version>${janus.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```

[:Fact:]{(io.sarl.sre.janus.network.SreNetworkVersion).MAVEN_GROUP_ID == '[:networkextensiongroup!]'}

[:Fact:]{(io.sarl.sre.janus.network.SreNetworkVersion).MAVEN_ARTIFACT_ID == '[:networkextensionname!]'}

The [:networkextensionname:] maven module provides the network extension **AND** all the libraries related to the Janus SRE.


## Configuration of the plugin

The following table provides a short description of the configuration parameters that are related to this networking
extension.
More details could be obtain by calling your Janus-based software with the command-line option `-H` for example.


| Section | Property | Type | Description |
| ------- | -------- | ---- | ----------- |
[:Dynamic:]{
	runShellSilently(whichShellCommand("janusnode"),
		"generatemarkdownconfighelp".makeCliOption,
		"generatemarkdownconfighelp.root".makeCliDefinition("srenetwork"))
}


[:Include:](../includes/legal.inc)

