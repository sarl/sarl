/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Collections;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.TreeMap;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugin.PluginConfigurationException;
import org.apache.maven.plugin.PluginManagerException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugin.logging.Log;

/** This class permits to support the incompatible Maven API
 * from the same Mojo code (says 3.0 and 3.1 APIs).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MavenHelper {

	private static Map<String, Dependency> pluginDependencies;

	private final MavenSession session;
	private final BuildPluginManager buildPluginManager;
	private final Log log;

	private final Method getRepositorySessionMethod;
	private final Method loadPluginMethod;

	/**
	 * @param session - the Maven session.
	 * @param buildPluginManager - the Maven build plugin manager.
	 * @param log - the log for the caller.
	 * @throws MojoExecutionException if cannot get the accessors.
	 */
	public MavenHelper(MavenSession session, BuildPluginManager buildPluginManager, Log log)
			throws MojoExecutionException {
		this.session = session;
		this.buildPluginManager = buildPluginManager;
		this.log = log;

		Method method;

		method = null;
		for (Method m : this.session.getClass().getDeclaredMethods()) {
			if ("getRepositorySession".equals(m.getName())) { //$NON-NLS-1$
				method = m;
				break;
			}
		}
		if (method == null) {
			throw new MojoExecutionException("Unsupported Maven API", //$NON-NLS-1$
					new NoSuchMethodError("getRepositorySystem")); //$NON-NLS-1$
		}
		this.getRepositorySessionMethod = method;

		method = null;
		for (Method m : this.buildPluginManager.getClass().getDeclaredMethods()) {
			if ("loadPlugin".equals(m.getName())) { //$NON-NLS-1$
				method = m;
				break;
			}
		}
		if (method == null) {
			throw new MojoExecutionException("Unsupported Maven API", //$NON-NLS-1$
					new NoSuchMethodError("loadPlugin")); //$NON-NLS-1$
		}
		this.loadPluginMethod = method;
	}

	/** Replies the current Maven session.
	 *
	 * @return the session.
	 */
	public MavenSession getSession() {
		return this.session;
	}

	/** Replies the manager of the build plugins.
	 *
	 * @return the manager of the build plugins.
	 */
	public BuildPluginManager getBuildPluginManager() {
		return this.buildPluginManager;
	}

	/** Extract the value from the hard-coded configuration.
	 *
	 * @param key - the key of the configuration entry.
	 * @return the value.
	 * @throws MojoExecutionException on error.
	 */
	public static String getConfig(String key) throws MojoExecutionException {
		ResourceBundle resource = null;
		try {
			resource = ResourceBundle.getBundle(
					"io/sarl/maven/compiler/config", //$NON-NLS-1$
					java.util.Locale.getDefault(),
					MavenHelper.class.getClassLoader());
		} catch (MissingResourceException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
		String value = resource.getString(key);
		if (value == null || value.isEmpty()) {
			throw new MojoExecutionException("Cannot find the configuration entry: " + key); //$NON-NLS-1$
		}
		return value;
	}

	/** Load the given plugin.
	 *
	 * @param plugin - the plugin to load.
	 * @return the descriptor of the plugin.
	 * @throws MojoExecutionException if something bad append.
	 */
	public PluginDescriptor loadPlugin(Plugin plugin)
			throws MojoExecutionException {
		try {
			Object repositorySessionObject = this.getRepositorySessionMethod.invoke(this.session);
			return (PluginDescriptor) this.loadPluginMethod.invoke(
					this.buildPluginManager,
					plugin,
					Collections.EMPTY_LIST,
					repositorySessionObject);
		} catch (IllegalAccessException | IllegalArgumentException
				| InvocationTargetException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
	}

	/** Execute the given mojo.
	 *
	 * @param mojo - the mojo to execute.
	 * @throws MojoExecutionException if the mojo cannot be run properly.
	 * @throws MojoFailureException if the build failed.
	 */
	public void executeMojo(MojoExecution mojo) throws MojoExecutionException, MojoFailureException {
		try {
			this.buildPluginManager.executeMojo(this.session, mojo);
		} catch (PluginConfigurationException
				| PluginManagerException e) {
			throw new MojoFailureException(e.getLocalizedMessage(), e);
		}
	}

	/** Convert an artifact to a dependency.
	 *
	 * @param artifact - the artifact to convert.
	 * @return the result of the conversion.
	 */
	@SuppressWarnings("static-method")
	public Dependency toDependency(Artifact artifact) {
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

	/** Build the map of dependencies for the current plugin.
	 *
	 * @param mojoGoal - name of the mojo goal.
	 * @return the artifact.
	 * @throws MojoExecutionException if the current plugin cannot be determined.
	 */
	public static Map<String, Dependency> getPluginDependencies(String mojoGoal) throws MojoExecutionException {
		synchronized (MavenHelper.class) {
			if (pluginDependencies == null) {
				Map<String, Dependency> deps = new TreeMap<>();
				String dependencies = getConfig("plugin.dependencies"); //$NON-NLS-1$
				Pattern p = Pattern.compile("^\\s*\\[\\s*(.*?)\\s*\\]\\s*$", Pattern.DOTALL); //$NON-NLS-1$
				Matcher m = p.matcher(dependencies);
				if (m.matches()) {
					dependencies = m.group(1);
					p = Pattern.compile("Dependency\\s*\\{\\s*(.+?)\\s*\\}", Pattern.DOTALL); //$NON-NLS-1$
					m = p.matcher(dependencies);
					while (m.find()) {
						String element = m.group(1);
						Dependency dep = new Dependency();
						dep.setClassifier(""); //$NON-NLS-1$
						dep.setOptional(false);
						dep.setScope("compile"); //$NON-NLS-1$
						String groupId = null;
						String artifactId = null;
						for (String entry : element.split(",\\s")) { //$NON-NLS-1$
							String[] pair = entry.trim().split("="); //$NON-NLS-1$
							switch(pair[0]) {
							case "groupId": //$NON-NLS-1$
								groupId = pair[1].trim();
								dep.setGroupId(groupId);
								break;
							case "artifactId": //$NON-NLS-1$
								artifactId = pair[1].trim();
								dep.setArtifactId(artifactId);
								break;
							case "version": //$NON-NLS-1$
								dep.setVersion(pair[1].trim());
								break;
							case "type": //$NON-NLS-1$
								dep.setType(pair[1].trim());
								break;
							default:
								throw new MojoExecutionException(
										"Invalid format of the 'plugin.dependencies' configuration value"); //$NON-NLS-1$
							}
						}
						if (groupId == null || artifactId == null) {
							throw new MojoExecutionException(
									"Invalid format of the 'plugin.dependencies' configuration value"); //$NON-NLS-1$
						}
						deps.put(ArtifactUtils.versionlessKey(groupId, artifactId), dep);
					}
				} else {
					throw new MojoExecutionException(
							"Invalid format of the 'plugin.dependencies' configuration value"); //$NON-NLS-1$
				}

				pluginDependencies = deps;
			}
			return pluginDependencies;
		}
	}

	/** Replies the version of the given plugin that is specified in the POM of the
	 * plugin in which this mojo is located.
	 *
	 * @param groupId - the identifier of the group.
	 * @param artifactId - thidentifier of the artifact.
	 * @param mojoGoal - name of the mojo goal.
	 * @return the version, never <code>null</code>
	 * @throws MojoExecutionException if the plugin was not found.
	 */
	public String getPluginDependencyVersion(String groupId, String artifactId, String mojoGoal) throws MojoExecutionException {
		Map<String, Dependency> deps = getPluginDependencies(mojoGoal);
		String key = ArtifactUtils.versionlessKey(groupId, artifactId);
		this.log.debug("COMPONENT DEPENDENCIES(getPluginVersionFromDependencies):"); //$NON-NLS-1$
		this.log.debug(deps.toString());
		Dependency dep = deps.get(key);
		if (dep != null) {
			String version = dep.getVersion();
			if (version != null && !version.isEmpty()) {
				return version;
			}
			throw new MojoExecutionException("Cannot determine the version for the plugin " + key); //$NON-NLS-1$
		}
		throw new MojoExecutionException("Cannot find the plugin " + key //$NON-NLS-1$
				+ " in dependencies: " + deps); //$NON-NLS-1$
	}

}
