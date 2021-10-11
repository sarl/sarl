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

import java.io.File;
import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.Map;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.base.Throwables;
import com.google.common.collect.Iterables;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.MojoDescriptor;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.codehaus.plexus.configuration.PlexusConfigurationException;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomUtils;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.compiler.batch.CompilerStatus;
import io.sarl.lang.compiler.batch.IJavaBatchCompiler;
import io.sarl.lang.compiler.batch.OptimizationLevel;

/** Java batch compiler based on the Maven definition of a Java compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
final class MavenBatchCompiler implements IJavaBatchCompiler {

	private static final String DEFAULT_COMPILER_VERSION = "3.8.1"; //$NON-NLS-1$

	private static final String MAVEN_COMPILER_PLUGIN_GROUPID = "org.apache.maven.plugins"; //$NON-NLS-1$

	private static final String MAVEN_COMPILER_PLUGIN_ARTIFACTID = "maven-compiler-plugin"; //$NON-NLS-1$

	private static final String MAVEN_COMPILER_PLUGIN_STANDARD_GOAL = "compile"; //$NON-NLS-1$

	private static final String MAVEN_COMPILER_PLUGIN_TEST_GOAL = "testCompile"; //$NON-NLS-1$

	private final MavenHelper helper;

	private final boolean isTestContext;

	/** Constructor.
	 *
	 * @param helper the Maven helper.
	 * @param isTestContext indicates if the compiler is used for test code.
	 */
	MavenBatchCompiler(MavenHelper helper, boolean isTestContext) {
		this.helper = helper;
		this.isTestContext = isTestContext;
	}

	@Override
	public String getName() {
		return "Maven compiler"; //$NON-NLS-1$
	}

	@Override
	@SuppressWarnings({"checkstyle:parameternumber"})
	public CompilerStatus compile(File classDirectory,
			Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries,
			Iterable<File> modulePathEntries,
			String javaVersion,
			String encoding,
			boolean isCompilerMoreVerbose,
			OptimizationLevel optimizationLevel,
			PrintWriter outWriter,
			PrintWriter errWriter,
			Logger logger,
			IProgressMonitor progress) {
		try {
			final Map<String, Plugin> declaredPlugins = this.helper.getSession().getCurrentProject().getBuild().getPluginsAsMap();
			Plugin plugin = declaredPlugins.get(ArtifactUtils.versionlessKey(MAVEN_COMPILER_PLUGIN_GROUPID, MAVEN_COMPILER_PLUGIN_ARTIFACTID));
			if (plugin == null) {
				// No maven-compiler-plugin declared within the project.
				plugin = new Plugin();
				plugin.setArtifactId(MAVEN_COMPILER_PLUGIN_ARTIFACTID);
				plugin.setGroupId(MAVEN_COMPILER_PLUGIN_GROUPID);
				plugin.setVersion(findVersion(logger));
			}
			final PluginDescriptor pluginDescriptor = this.helper.loadPlugin(plugin);
			if (pluginDescriptor != null) {
				final String goal = this.isTestContext
						? MAVEN_COMPILER_PLUGIN_TEST_GOAL
						: MAVEN_COMPILER_PLUGIN_STANDARD_GOAL;
				final MojoDescriptor mojoDescriptor = pluginDescriptor.getMojo(goal);
				if (mojoDescriptor != null) {
					final Xpp3Dom mojoConfiguration;
					try {
						mojoConfiguration = this.helper.toXpp3Dom(mojoDescriptor.getMojoConfiguration());
					} catch (PlexusConfigurationException e1) {
						throw new MojoExecutionException(e1.getLocalizedMessage(), e1);
					}

					final Xpp3Dom pluginConfiguration = (Xpp3Dom) plugin.getConfiguration();

					final Xpp3Dom specificConfiguration = new Xpp3Dom("configuration"); //$NON-NLS-1$
					Xpp3Dom child;
					if (!Strings.isEmpty(javaVersion)) {
						child = new Xpp3Dom("source"); //$NON-NLS-1$
						child.setValue(javaVersion);
						specificConfiguration.addChild(child);
						child = new Xpp3Dom("target"); //$NON-NLS-1$
						child.setValue(javaVersion);
						specificConfiguration.addChild(child);
					}
					if (!Strings.isEmpty(encoding)) {
						child = new Xpp3Dom("encoding"); //$NON-NLS-1$
						child.setValue(encoding);
						specificConfiguration.addChild(child);
					}
					child = new Xpp3Dom("outputDirectory"); //$NON-NLS-1$
					child.setValue(classDirectory.getAbsolutePath());
					specificConfiguration.addChild(child);

					Xpp3Dom configuration = specificConfiguration;
					if (pluginConfiguration != null) {
						configuration = Xpp3DomUtils.mergeXpp3Dom(configuration, pluginConfiguration);
					}
					if (mojoConfiguration != null) {
						configuration = Xpp3DomUtils.mergeXpp3Dom(configuration, mojoConfiguration);
					}

					final MojoExecution execution = new MojoExecution(mojoDescriptor, configuration);
					this.helper.executeMojo(execution);
					return CompilerStatus.COMPILATION_SUCCESS;
				}
			}
		} catch (Exception exception) {
			if (logger != null) {
				logger.log(Level.SEVERE, exception.getLocalizedMessage(), Throwables.getRootCause(exception));
			}
		}
		return CompilerStatus.COMPILATION_FAILURE;
	}

	private String findVersion(Logger logger) throws MojoExecutionException {
		final Set<Artifact> artifacts = this.helper.resolve(
				MAVEN_COMPILER_PLUGIN_GROUPID,
				MAVEN_COMPILER_PLUGIN_ARTIFACTID);
		final Artifact pluginArtifact = Iterables.find(artifacts, it -> MAVEN_COMPILER_PLUGIN_ARTIFACTID.equals(it.getArtifactId())
				&& MAVEN_COMPILER_PLUGIN_GROUPID.equals(it.getGroupId()));
		if (pluginArtifact != null) {
			return pluginArtifact.getVersion();
		}
		logger.warning(MessageFormat.format(Messages.MavenBatchCompiler_0, DEFAULT_COMPILER_VERSION));
		return DEFAULT_COMPILER_VERSION;
	}

}
