# How to add an example

Let the example having the following identifiers:
* `io-sarl-demos-myexample`
* `io_sarl_demos_myexample`
* `io.sarl.examples.myexample`


## 1. Include the example code

1. Create the folder `projects/io-sarl-demos-myexample`.
2. Copy the example's code into the created folder.


## 2. Create the wizard for importing the example

Add in the `plugin.xml` file:


	<!-- ========= io-sarl-demos-myexample Example ========= -->
	
	<extension point="org.eclipse.ui.newWizards">
		<wizard id="io.sarl.examples.myexample"
			name="%io_sarl_demos_myexample_Name"
			class="io.sarl.examples.SARLExampleExecutableExtensionFactory:io.sarl.examples.wizard.SarlExampleInstallerWizard"
			category="org.eclipse.ui.Examples/io.sarl.Examples"
			icon="platform:/plugin/org.eclipse.xtend.examples/icons/genproject.gif"
			project="true">
			<description>%io_sarl_demos_myexample_Description</description>
		</wizard>
	</extension>
	
	<extension point="org.eclipse.ui.newWizards">
		<wizard id="io.sarl.examples.myexample"
			name="%io_sarl_demos_myexample_Name"
			class="io.sarl.examples.SARLExampleExecutableExtensionFactory:io.sarl.examples.wizard.SarlExampleInstallerWizard"
			category="io.sarl.eclipse.category.wizards/io.sarl.project.Examples"
			icon="platform:/plugin/org.eclipse.xtend.examples/icons/genproject.gif"
			project="true">
			<description>%io_sarl_demos_myexample_Description</description>
		</wizard>
	</extension>
	
	<extension point="org.eclipse.emf.common.ui.examples">
		<example wizardID="io.sarl.examples.myexample" pageImage="platform:/plugin/org.eclipse.xtend.examples/icons/genproject.gif">
			<projectDescriptor name="io-sarl-demos-myexample" contentURI="contents/io-sarl-demos-myexample.zip" description="%io_sarl_demos_myexample_Description"/>
			<fileToOpen location="path/to/the/file/to/open"/>
		</example>
	</extension>


Replace `path/to/the/file/to/open` by the path to the file to be opened after importing the example.


## 3. Update the localized strings.

The update explained in the previous section uses two strings in the localized properties.
Add in the `OSGI-INF/bundle.properties` file:


	io_sarl_demos_myexample_Name = THE NAME OF THE EXAMPLE
	io_sarl_demos_myexample_Description = THE DESCRIPTION OF THE EXAMPLE



## 4. Add a quick link in the welcome pages.

In order to add a quick link to the example, you should add  into the `plugin.xml` file:


	<!-- adding a new quicklink for the example -->
	<extension
	      point="org.eclipse.ui.intro.quicklinks">
	      <command
	       	id="org.eclipse.ui.newWizard(newWizardId=io.sarl.examples.myexample)"
	         description="%io_sarl_demos_myexample_Description"
	         label="%io_sarl_demos_myexample_Name"
	         icon="platform:/plugin/org.eclipse.ui.intro.universal/themes/circles/graphics/icons/ctool/sa_onesample48.gif">
	      </command>
	</extension>



## 5. Add the example into the welcome page

Add into the `SARL-Example-Intro.xml` file:


	<group style-id="quick-links" id="quick-links">
		<link style-id="content-link" label="Description of the example in the welcome page"
			url="http://org.eclipse.ui.intro/execute?command=org.eclipse.ui.newWizard%28newWizardId%3Dio.sarl.examples.myexample%29"
			id="io.sarl.examples.myexample">
			<text>
				Detailed description of the example.
			</text>
		</link>
	</group>



## 6. Update the building script

Add into the `src-templates/build.xml` file:


	<zip_example name="io-sarl-demos-myexample" />


## 7. Add launching configuration

It is possible to specify how the demo could be launch by providing a `launch.xml` file.
Two cases: launch a Java application that embeds SARL, or launch agents.


### 7.1. Launch a Java Application

The content of the `launch.xml` file should be similar to:

	<?xml version="1.0" encoding="UTF-8"?>
	<launchConfigurations id="**ID**">
		<application class="**CLASSNAME**" name="**NAME**" />
	</launchConfigurations>


`**ID** is the identifier of the example; **CLASSNAME** the fully qualified name of the
Java class that contains the `main()` function; and `**NAME**` is the name of the example.



### 7.2. Launch Agents

The content of the `launch.xml` file should be similar to:

	<?xml version="1.0" encoding="UTF-8"?>
	<launchConfigurations id="**ID**">
		<agent class="**CLASSNAME**" name="**NAME**" />
		<agent class="**CLASSNAME**" name="**NAME**" />
		...
	</launchConfigurations>


`**ID** is the identifier of the example; **CLASSNAME** the fully qualified name of the
agent to launch; and `**NAME**` is the name of the example.


## 8. Add an on-line documentation

Create the documentation description `documentation.xml` file:

	<?xml version="1.0" encoding="UTF-8"?>
	<documentation id="**ID**"
	               name="**NAME**"
	               category="**CATEGORY**">
		<mainPage>**MAIN**</mainPage>
		<screenshot>**SCREENSHOT**</screenshot>
		<description>**DESCRIPTION**</description>
		<resources>
			<resource>**FILENAME**</resource>
			<resource>**FILENAME**</resource>
			...
		</resources>
	</documentation>

* `**ID**` is the identifier of the example;
* `**NAME**` is the name of the example;
* `**CATEGORY**` is the fully qualified name of the category (website dependent);
* `**MAIN**` is the filename (relative to `documentation.xml`) to the main documentation file (usually a Markdown or HTML file);
* `**SCREENSHOT**` is the filename (relative to `documentation.xml`) to a screnshot picture for the example;
* `**DESCRIPTION**` is a text that describe the example; and
* `**FILENAME**` is the filename  (relative to `documentation.xml`) of a file that must be included into the documentation of the example.


## 9. Regenerate files

Run: `mvn clean install`

