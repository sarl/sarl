# Create your First Project

[:Outline:]

For developing with SARL, you should create a project. This document describes two ways for created SARL projects.

Two ways are available for creating a SARL project:
1. creating a SARL project inside Eclipse without Maven, or
2. creating a SARL project with Maven (inside or outside Eclipse).

These two ways are explained below.

## Create a SARL Project without Maven

For creating a project, you should open your Eclipse and click on **File > New > Projects**, and select *SARL Project* in
the SARL category.

![Select the SARL Project Type](./new_sarl_project_screen_1.png)

After clicking on **Next**, the wizard is displaying the first page for creating a SARL project.


### Step 1: Entering the project information

You must enter the name of your project. You could change the standard SARL and Java environment configurations as well.


![Enter the Project Information](./new_sarl_project_screen_2.png)


Then you could click on **Next** for continuing the edition of the project's properties, or simply click on the
**Finish** button for creating the project with the default properties.

The rest of this section is devoted to the edition of the additional properties for the SARL project.


### Step 2: Configuration of the source folders"

The second page of the wizard contains the building settings. Two tabs are really interesting: the *Source* and the *Libraries*.

The *Source* tab defines the folders in your project that must contains source code files. By default, a SARL project is
composed of four source folders:

* [:src1:]: for your Java classes; [:Fact:]{io.sarl.lang.SARLConfig::FOLDER_SOURCE_JAVA == "[:src1](src/main/java)"}
* [:src2:]: for your SARL scripts; [:Fact:]{io.sarl.lang.SARLConfig::FOLDER_SOURCE_SARL == "[:src2](src/main/sarl)"}
* [:src3:]: for the Java codes generated by the SARL compiler (you should not change them yourself); [:Fact:]{io.sarl.lang.SARLConfig::FOLDER_SOURCE_GENERATED == "[:src3](src/main/generated-sources/sarl)"}
* [:src4:]: for the files that are not SARL nor Java code. [:Fact:]{io.sarl.lang.SARLConfig::FOLDER_RESOURCES == "[:src4](src/main/resources)"}

The default output folder is [:src5:]. [:Fact:]{io.sarl.lang.SARLConfig::FOLDER_BIN == "[:src5](target/classes)"}

> **_Note:_** The names of these folders are following the conventions of a Maven-based project (described below). In this way, you will
> be able to turn the Maven nature on your SARL project on/off.

![Source Code Folders](./new_sarl_project_screen_3.png)

## Create a SARL Project with Maven

For creating a project with both the Maven and SARL natures, you should open your Eclipse and click on
**File > New > Others > Maven > Maven Project**.

Follow the steps of the project creation wizard, and finally click on the **Finish** button.

### Edit the Maven configuration

Open the file `pom.xml`, and edit it for obtaining a content similar to the configuration below.

Replace the version number [:sarl.version:] of SARL with the one you want to use. You could search on the
[Maven Central Repository](http://search.maven.org/) for the last available version.

```xml
 <project>
    ...
    <properties>
       ...
       <sarl.version>[:sarl.version!]</sarl.version>
	   <jdk.version>[:sarl-user.min.jdk.version!]</jdk.version>
	   <project.build.sourceEncoding>[:project.encoding!]</project.build.sourceEncoding>
    </properties>
    ...
    <build>
       <plugins>
          ...
		  <plugin>
			<groupId>org.apache.maven.plugins</groupId>
			<artifactId>maven-compiler-plugin</artifactId>
			<version>[:maven.compiler.version!]</version>
			<configuration>
				<source>${jdk.version}</source>
				<target>${jdk.version}</target>
				<encoding>${project.build.sourceEncoding}</encoding>
			</configuration>
		  </plugin>
          <plugin>
             <groupId>[:plugingroupid](io.sarl.lang)</groupId>
             <artifactId>[:pluginname](sarl-maven-plugin)</artifactId>
             <version>${sarl.version}</version>
             <extensions>true</extensions>
             <configuration>
                <source>${jdk.version}</source>
                <encoding>${project.build.sourceEncoding}</encoding>
             </configuration>
          </plugin>
       </plugins>
    </build>
    ...
    <dependencies>
      ...
       <dependency>
          <groupId>[:sarlsdkgroupid](io.sarl.sdk)</groupId>
          <artifactId>[:sarlsdkartifactid](sdk)</artifactId>
          <version>${sarl.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlMavenPluginGroupId(null) == '[:plugingroupid!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlMavenPluginArtifactId(null) == '[:pluginname!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlSdkGroupId(null) == '[:sarlsdkgroupid!]'}

[:Fact:]{(io.sarl.lang.maven.compiler.utils.MavenHelper).getSarlSdkArtifactId(null) == '[:sarlsdkartifactid!]'}

The Maven configuration is based on the use of [:pluginname:]. This plugin is in charge of compiling the SARL and
the Java files. Details about the [:pluginname:] may be found on [this page](../tools/MavenSarlPlugin.md).

> **_Important Note:_** You must set the `extensions` tag to true for the [:pluginname:] plugin. If you missed to set it, the plugin
> will not able to be integrated in the Maven life-cycle. The consequence will be that only the Java compiler will be
> invoked.

### Configuration of a runtime environment (optional)

For executing your SARL program, a [run-time environment]([:sarl.url!]/runtime/index.html) should be used.
By default, the SARL development environment replaces any reference to the SARL libraries by the run-time environment's libraries
when the SARL program is launched within the SARL environment or when a runnable Jar library is created.

In several specific cases, you may want to include the runtime environment into the Maven dependencies of your project. In
this case, you could replace the Maven dependency to the SARL sdk (as defined in the previous section) by a Maven dependency
to the runtime environment library.

> **_Caution:_** Replacing the SARL sdk library by the SARL run-time environment (SRE) library within the Maven dependencies is not the
> recommended approach by the SARL core developers.

#### Janus as maven dependency

The runtime environment that is recommended by the developers of SARL is [Janus](http://www.sarl.io/runtime/janus/). 

Replace the version number ([:janus.version:]) of the [Janus platform](http://www.sarl.io/runtime/janus/) with the one you want to use.
You could search on the [Maven Central Repository](http://search.maven.org/) for the last available version.


```xml
 <project>
   ...
    <properties>
       ...
       <janus.version>[:janus.version!]</janus.version>
    </properties>
    ...
    <build>
       <plugins>
          ...
          <plugin>
             <groupId>[:plugingroupid!]</groupId>
             <artifactId>[:pluginname!]</artifactId>
             <version>${sarl.version}</version>
             <extensions>true</extensions>
             <configuration>
                <source>${jdk.version}</source>
                <encoding>[:project.encoding!]</encoding>
             </configuration>
          </plugin>
       </plugins>
    </build>
    ...
    <dependencies>
       ...
       <dependency>
          <groupId>[:janusgroupid](io.sarl.sre.janus)</groupId>
          <artifactId>[:janusartifactid](janus.kernel)</artifactId>
          <version>${janus.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```

[:Fact:]{(io.sarl.sre.janus.SreVersion).MAVEN_GROUP_ID == '[:janusgroupid!]'}

[:Fact:]{(io.sarl.sre.janus.SreVersion).MAVEN_ARTIFACT_ID == '[:janusartifactid!]'}


#### Janus with networking feature as maven dependency

By default, the Janus framework does not activate its network feature. If you would like to build a project with this feature,
you have to use another Maven dependency: [:janusnetworkplugin:].

```xml
 <project>
   ...
    <properties>
       ...
       <janus.version>[:janus.version!]</janus.version>
    </properties>
    ...
    <build>
       <plugins>
          ...
          <plugin>
             <groupId>[:plugingroupid!]</groupId>
             <artifactId>[:pluginname!]</artifactId>
             <version>${sarl.version}</version>
             <extensions>true</extensions>
             <configuration>
                <source>${jdk.version}</source>
                <encoding>[:project.encoding!]</encoding>
             </configuration>
          </plugin>
       </plugins>
    </build>
    ...
    <dependencies>
       ...
       <dependency>
          <groupId>io.sarl.sre.janus</groupId>
          <artifactId>[:janusnetworkplugin](janus.network)</artifactId>
          <version>${janus.version}</version>
       </dependency>
       ...
    </dependencies>
    ...
 </project>
```

## What's next?

In the next section, we will learn how to create our first agent.

[Next>](./AgentIntroduction.md)

[:Include:](../includes/legal.inc)

