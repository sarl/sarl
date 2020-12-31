/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

import java.io.StringReader;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.HashSet;
import java.util.Map;
import java.util.MissingResourceException;
import java.util.ResourceBundle;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.base.Strings;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.resolver.ArtifactResolutionException;
import org.apache.maven.artifact.resolver.ArtifactResolutionRequest;
import org.apache.maven.artifact.resolver.ArtifactResolutionResult;
import org.apache.maven.artifact.resolver.MultipleArtifactsNotFoundException;
import org.apache.maven.artifact.resolver.ResolutionErrorHandler;
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
import org.apache.maven.repository.RepositorySystem;
import org.codehaus.plexus.configuration.PlexusConfiguration;
import org.codehaus.plexus.configuration.PlexusConfigurationException;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomBuilder;

/** This class permits to support the incompatible Maven API
 * from the same Mojo code (says 3.0 and 3.1 APIs).
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class MavenHelper {

	private Map<String, Dependency> pluginDependencies;

	private final MavenSession session;

	private final BuildPluginManager buildPluginManager;

	private final Log log;

	private final Method getRepositorySessionMethod;

	private final Method loadPluginMethod;

	private final RepositorySystem repositorySystem;

	private final ResolutionErrorHandler resolutionErrorHandler;

	/** Constructor.
	 * @param session the Maven session.
	 * @param buildPluginManager the Maven build plugin manager.
	 * @param repositorySystem the Repository system.
	 * @param resolutionErrorHandler the error handler during artifact resolution.
	 * @param log the log for the caller.
	 * @throws MojoExecutionException if cannot get the accessors.
	 */
	MavenHelper(MavenSession session, BuildPluginManager buildPluginManager, RepositorySystem repositorySystem,
			ResolutionErrorHandler resolutionErrorHandler, Log log) throws MojoExecutionException {
		this.session = session;
		this.buildPluginManager = buildPluginManager;
		this.log = log;
		this.repositorySystem = repositorySystem;
		this.resolutionErrorHandler = resolutionErrorHandler;

		Method method;

		method = null;
		for (final Method m : this.session.getClass().getDeclaredMethods()) {
			if ("getRepositorySession".equals(m.getName())) { //$NON-NLS-1$
				method = m;
				break;
			}
		}
		if (method == null) {
			throw new MojoExecutionException(Messages.MavenHelper_0, new NoSuchMethodError("getRepositorySystem")); //$NON-NLS-1$
		}
		this.getRepositorySessionMethod = method;

		method = null;
		for (final Method m : this.buildPluginManager.getClass().getDeclaredMethods()) {
			if ("loadPlugin".equals(m.getName())) { //$NON-NLS-1$
				method = m;
				break;
			}
		}
		if (method == null) {
			throw new MojoExecutionException(Messages.MavenHelper_0, new NoSuchMethodError("loadPlugin")); //$NON-NLS-1$
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
	 * @param key the key of the configuration entry.
	 * @return the value.
	 * @throws MojoExecutionException on error.
	 */
	public String getConfig(String key) throws MojoExecutionException {
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
			value = Strings.nullToEmpty(value);
			this.log.warn(MessageFormat.format(Messages.MavenHelper_1, key));
		}
		return value;
	}

	/** Load the given plugin.
	 *
	 * @param plugin the plugin to load.
	 * @return the descriptor of the plugin.
	 * @throws MojoExecutionException if something bad append.
	 */
	public PluginDescriptor loadPlugin(Plugin plugin)
			throws MojoExecutionException {
		try {
			final Object repositorySessionObject = this.getRepositorySessionMethod.invoke(this.session);
			return (PluginDescriptor) this.loadPluginMethod.invoke(
					this.buildPluginManager,
					plugin,
					getSession().getCurrentProject().getRemotePluginRepositories(),
					repositorySessionObject);
		} catch (IllegalAccessException | IllegalArgumentException
				| InvocationTargetException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
	}

	/** Execute the given mojo.
	 *
	 * @param mojo the mojo to execute.
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
	 * @param artifact the artifact to convert.
	 * @return the result of the conversion.
	 */
	@SuppressWarnings("static-method")
	public Dependency toDependency(Artifact artifact) {
		final Dependency result = new Dependency();
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
	 * @return the artifact.
	 * @throws MojoExecutionException if the current plugin cannot be determined.
	 */
	public synchronized Map<String, Dependency> getPluginDependencies() throws MojoExecutionException {
		if (this.pluginDependencies == null) {
			final String groupId = getConfig("plugin.groupId"); //$NON-NLS-1$
			final String artifactId = getConfig("plugin.artifactId"); //$NON-NLS-1$
			final String pluginArtifactKey = ArtifactUtils.versionlessKey(groupId, artifactId);

			final Set<Artifact> dependencies = resolveDependencies(pluginArtifactKey, true);

			final Map<String, Dependency> deps = new TreeMap<>();

			for (final Artifact artifact : dependencies) {
				final Dependency dep = toDependency(artifact);
				deps.put(ArtifactUtils.versionlessKey(artifact), dep);
			}

			this.pluginDependencies = deps;
		}
		return this.pluginDependencies;
	}

	/** Resolve an artifact.
	 *
	 * @param request the definition of the resolution request.
	 * @return the result.
	 * @throws MojoExecutionException if the resolution cannot be done.
	 * @since 0.8
	 */
	public ArtifactResolutionResult resolve(ArtifactResolutionRequest request) throws MojoExecutionException {
		return this.repositorySystem.resolve(request);
	}

	/** Resolve the artifacts with the given key.
	 *
	 * @param groupId the group identifier.
	 * @param artifactId the artifact identifier.
	 * @return the discovered artifacts.
	 * @throws MojoExecutionException if resolution cannot be done.
	 * @since 0.8
	 */
	public Set<Artifact> resolve(String groupId, String artifactId) throws MojoExecutionException {
		final ArtifactResolutionRequest request = new ArtifactResolutionRequest();
		request.setResolveRoot(true);
		request.setResolveTransitively(true);
		request.setLocalRepository(getSession().getLocalRepository());
		request.setRemoteRepositories(getSession().getCurrentProject().getRemoteArtifactRepositories());
		request.setOffline(getSession().isOffline());
		request.setForceUpdate(getSession().getRequest().isUpdateSnapshots());
		request.setServers(getSession().getRequest().getServers());
		request.setMirrors(getSession().getRequest().getMirrors());
		request.setProxies(getSession().getRequest().getProxies());
		request.setArtifact(createArtifact(groupId, artifactId));

		final ArtifactResolutionResult result = resolve(request);

		return result.getArtifacts();
	}

	/** Create an instance of artifact with a version range that corresponds to all versions.
	 *
	 * @param groupId the group identifier.
	 * @param artifactId the artifact identifier.
	 * @return the artifact descriptor.
	 * @since 0.8
	 */
	public Artifact createArtifact(String groupId, String artifactId) {
		return this.repositorySystem.createArtifact(groupId, artifactId, "RELEASE", "jar"); //$NON-NLS-1$ //$NON-NLS-2$
	}

	/** Replies the dependencies for the given artifact.
	 *
	 * @param artifactId the artifact identifier.
	 * @param plugins indicates if the map of the plugin artifacts must be explore (if true) or the dependency
	 *     artifacts (if false).
	 * @return the dependencies.
	 * @throws MojoExecutionException if the resolution cannot be done.
	 */
	public Set<Artifact> resolveDependencies(String artifactId, boolean plugins) throws MojoExecutionException {
		final Artifact pluginArtifact;
		if (plugins) {
			pluginArtifact = getSession().getCurrentProject().getPluginArtifactMap().get(artifactId);
		} else {
			pluginArtifact = getSession().getCurrentProject().getArtifactMap().get(artifactId);
		}

		final ArtifactResolutionRequest request = new ArtifactResolutionRequest();
		request.setResolveRoot(false);
		request.setResolveTransitively(true);
		request.setLocalRepository(getSession().getLocalRepository());
		request.setRemoteRepositories(getSession().getCurrentProject().getRemoteArtifactRepositories());
		request.setOffline(getSession().isOffline());
		request.setForceUpdate(getSession().getRequest().isUpdateSnapshots());
		request.setServers(getSession().getRequest().getServers());
		request.setMirrors(getSession().getRequest().getMirrors());
		request.setProxies(getSession().getRequest().getProxies());
		request.setArtifact(pluginArtifact);

		final ArtifactResolutionResult result = resolve(request);

		try {
			this.resolutionErrorHandler.throwErrors(request, result);
		} catch (MultipleArtifactsNotFoundException e) {
			final Collection<Artifact> missing = new HashSet<>(e.getMissingArtifacts());
			if (!missing.isEmpty()) {
				throw new MojoExecutionException(e.getLocalizedMessage(), e);
			}
		} catch (ArtifactResolutionException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}

		return result.getArtifacts();
	}

	/** Replies the version of the given plugin that is specified in the POM of the
	 * plugin in which this mojo is located.
	 *
	 * @param groupId the identifier of the group.
	 * @param artifactId thidentifier of the artifact.
	 * @return the version, never {@code null}
	 * @throws MojoExecutionException if the plugin was not found.
	 */
	public String getPluginDependencyVersion(String groupId, String artifactId) throws MojoExecutionException {
		final Map<String, Dependency> deps = getPluginDependencies();
		final String key = ArtifactUtils.versionlessKey(groupId, artifactId);
		this.log.debug("COMPONENT DEPENDENCIES(getPluginVersionFromDependencies):"); //$NON-NLS-1$
		this.log.debug(deps.toString());
		final Dependency dep = deps.get(key);
		if (dep != null) {
			final String version = dep.getVersion();
			if (version != null && !version.isEmpty()) {
				return version;
			}
			throw new MojoExecutionException(MessageFormat.format(Messages.MavenHelper_2, key));
		}
		throw new MojoExecutionException(MessageFormat.format(Messages.MavenHelper_3, key, deps));
	}

	/** Convert a Plexus configuration to its XML equivalent.
	 *
	 * @param config the Plexus configuration.
	 * @return the XML configuration.
	 * @throws PlexusConfigurationException in case of problem.
	 * @since 0.8
	 */
	public Xpp3Dom toXpp3Dom(PlexusConfiguration config) throws PlexusConfigurationException {
		final Xpp3Dom result = new Xpp3Dom(config.getName());
		result.setValue(config.getValue(null));
		for (final String name : config.getAttributeNames()) {
			result.setAttribute(name, config.getAttribute(name));
		}
		for (final PlexusConfiguration child : config.getChildren()) {
			result.addChild(toXpp3Dom(child));
		}
		return result;
	}

	/** Parse the given string for extracting an XML tree.
	 *
	 * @param content the text to parse.
	 * @param logger the logger to use for printing out the parsing errors. May be {@code null}.
	 * @return the XML tree, or {@code null} if empty.
	 * @since 0.8
	 */
	@SuppressWarnings("static-method")
	public Xpp3Dom toXpp3Dom(String content, Log logger) {
		if (content != null && !content.isEmpty()) {
			try (StringReader sr = new StringReader(content)) {
				return Xpp3DomBuilder.build(sr);
			} catch (Exception exception) {
				if (logger != null) {
					logger.debug(exception);
				}
			}
		}
		return null;
	}

}
