/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.maven.compiler;

import java.io.File;
import java.io.IOException;
import java.io.StringReader;
import java.util.Arrays;
import java.util.Collections;
import java.util.MissingResourceException;
import java.util.ResourceBundle;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.InvalidPluginDescriptorException;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.PluginConfigurationException;
import org.apache.maven.plugin.PluginDescriptorParsingException;
import org.apache.maven.plugin.PluginManagerException;
import org.apache.maven.plugin.PluginNotFoundException;
import org.apache.maven.plugin.PluginResolutionException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.codehaus.plexus.component.repository.ComponentDependency;
import org.codehaus.plexus.configuration.PlexusConfiguration;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;
import org.codehaus.plexus.util.xml.Xpp3DomUtils;
import org.codehaus.plexus.util.xml.pull.XmlPullParserException;
import org.eclipse.aether.repository.RemoteRepository;

/** Abstract mojo for compiling SARL.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlMojo extends AbstractMojo {

	/**
	 * @parameter property="output" default-value="src/main/generated-sources/xtend/"
	 * @required
	 */
	protected File output;

	/**
	 * @parameter property="input" default-value="src/main/sarl/"
	 * @required
	 */
	protected File input;

	/**
	 * @parameter property="testOutput" default-value="src/test/generated-sources/xtend/"
	 * @required
	 */
	protected File testOutput;

	/**
	 * @parameter property="testInput" default-value="src/test/sarl/"
	 * @required
	 */
	protected File testInput;

	/**
	 * The current Maven session.
	 *
	 * @parameter default-value="${session}"
	 * @parameter required
	 * @readonly
	 */
	protected MavenSession session;

	/**
	 * The Build PluginManager component.
	 *
	 * @component
	 * @required
	 */
	protected BuildPluginManager buildPluginManager;

	/** Extract the value from the hard-coded configuration.
	 * 
	 * @param key - the key of the configuration entry.
	 * @return the value.
	 * @throws MojoExecutionException on error.
	 */
	protected String getConfig(String key) throws MojoExecutionException {
		ResourceBundle resource = null;
		try {
			resource = ResourceBundle.getBundle(
					"io/sarl/maven/compiler/config", //$NON-NLS-1$
					java.util.Locale.getDefault(),
					getClass().getClassLoader());
		}
		catch (MissingResourceException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
		String value = resource.getString(key);
		if (value == null || value.isEmpty()) {
			throw new MojoExecutionException("Cannot find the configuration entry: " + key); //$NON-NLS-1$
		}
		return value;
	}

	/** Replies the version of the given plugin that is specified in the POM of the
	 * plugin in which this mojo is located.
	 * 
	 * @param groupId - the identifier of the group.
	 * @param artifactId - thidentifier of the artifact.
	 * @return the version, never <code>null</code>
	 * @throws MojoExecutionException if the plugin was not found.
	 */
	protected String getPluginVersionFromDependencies(String groupId, String artifactId) throws MojoExecutionException {
		String key = ArtifactUtils.versionlessKey(groupId, artifactId);
		PluginDescriptor currentPlugin = (PluginDescriptor) getPluginContext().get("pluginDescriptor"); //$NON-NLS-1$
		for(ComponentDependency dep : currentPlugin.getDependencies()) {
			if (groupId.equals(dep.getGroupId())
					&& artifactId.equals(dep.getArtifactId())) {
				String version = dep.getVersion();
				if (version != null && !version.isEmpty()) {
					return version;
				}
				throw new MojoExecutionException("Cannot determine the version for the plugin " + key); //$NON-NLS-1$
			}
		}
		throw new MojoExecutionException("Cannot find the plugin " + key); //$NON-NLS-1$
	}

	/** Execute another MOJO.
	 *
	 * @param groupId - identifier of the MOJO plugin group.
	 * @param artifactId - identifier of the MOJO plugin artifact.
	 * @param version - version of the MOJO plugin version.
	 * @param goal - the goal to run.
	 * @param configuration - the XML code for the configuration.
	 * @param dependencies - the dependencies of the plugin.
	 * @throws MojoExecutionException when cannot run the MOJO.
	 */
	protected void executeDelegate(
			String groupId, String artifactId,
			String version, String goal,
			String configuration,
			Dependency... dependencies) throws MojoExecutionException {
		try {
			Plugin plugin = new Plugin();
			plugin.setArtifactId(artifactId);
			plugin.setGroupId(groupId);
			plugin.setVersion(version);
			plugin.setDependencies(Arrays.asList(dependencies));

			getLog().debug("Launching " + plugin.getId()); //$NON-NLS-1$

			PluginDescriptor pluginDescriptor = this.buildPluginManager.loadPlugin(
					plugin,
					Collections.<RemoteRepository>emptyList(),
					this.session.getRepositorySession());
			if (pluginDescriptor == null) {
				throw new MojoExecutionException("Could not find the plugin '" + plugin.getId() + "'"); //$NON-NLS-1$ //$NON-NLS-2$
			}
			MojoDescriptor mojoDescriptor = pluginDescriptor.getMojo(goal);
			if (mojoDescriptor == null) {
				throw new MojoExecutionException("Could not find the goal '" + goal + "'"); //$NON-NLS-1$ //$NON-NLS-2$
			}

			Xpp3Dom mojoXml = toXpp3Dom(mojoDescriptor.getMojoConfiguration());
			Xpp3Dom configurationXml = null;
			if (configuration != null && !configuration.isEmpty()) {
				try (StringReader sr = new StringReader(configuration)) {
					try {
						configurationXml = Xpp3DomBuilder.build(sr);
					} catch (XmlPullParserException | IOException e) {
						getLog().debug(e);
					}
				}
			}
			if (configurationXml != null) {
				configurationXml = Xpp3DomUtils.mergeXpp3Dom(
						configurationXml,
						mojoXml);
			} else {
				configurationXml = mojoXml;
			}

			getLog().debug("Configuration for " + plugin.getId() //$NON-NLS-1$
					+ "\n" + configurationXml.toString()); //$NON-NLS-1$

			MojoExecution execution = new MojoExecution(mojoDescriptor, configurationXml);

			this.buildPluginManager.executeMojo(this.session, execution);

		} catch (PluginNotFoundException | PluginResolutionException
				| PluginDescriptorParsingException
				| InvalidPluginDescriptorException
				| MojoFailureException | PluginConfigurationException
				| PluginManagerException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
	}

	private Xpp3Dom toXpp3Dom(PlexusConfiguration config) {
		Xpp3Dom result = new Xpp3Dom(config.getName());
		result.setValue(config.getValue(null));
		for (String name : config.getAttributeNames()) {
			result.setAttribute(name, config.getAttribute(name));
		}
		for (PlexusConfiguration child : config.getChildren()) {
			result.addChild(toXpp3Dom(child));
		}
		return result;
	}

	/** Convert an artifact to a dependency.
	 * 
	 * @param artifact - the artifact to convert.
	 * @return the result of the conversion.
	 */
	protected static Dependency toDependency(Artifact artifact) {
		Dependency result = new Dependency();
		result.setArtifactId(artifact.getArtifactId());
		result.setClassifier(artifact.getClassifier());
		result.setGroupId(artifact.getGroupId());
		result.setOptional(artifact.isOptional());
		result.setScope(artifact.getScope());
		result.setType(artifact.getType());
		result.setVersion(artifact.getVersion());
		return result;
	}

}
