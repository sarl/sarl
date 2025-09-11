/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

package io.sarl.lang.maven.compiler.abstractmojos;

import java.io.File;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.regex.Pattern;

import javax.inject.Inject;

import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.artifact.resolver.ResolutionErrorHandler;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.model.Dependency;
import org.apache.maven.model.Plugin;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.BuildPluginManager;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.repository.RepositorySystem;
import org.arakhne.afc.vmutil.FileSystem;
import org.codehaus.plexus.configuration.PlexusConfigurationException;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.codehaus.plexus.util.xml.Xpp3DomUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.maven.compiler.utils.MavenHelper;

/** Abstract mojo for SARL.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlMojo extends AbstractMojo {

	/** Environment variable that is defined in Maven for skipping tests.
	 *
	 * @since 0.12
	 */
	public static final String MAVEN_TEST_SKIP_NAME = "maven.test.skip"; //$NON-NLS-1$

	/** Environment variable that is defined in SARL for skipping tests.
	 *
	 * @since 0.12
	 */
	public static final String SARL_TEST_SKIP_NAME = "sarl.test.skip"; //$NON-NLS-1$

	/** Environment variable that is defined in SARL for skipping compilation of code (excluding tests).
	 *
	 * @since 0.12
	 */
	public static final String SARL_COMPILE_SKIP_NAME = "sarl.compile.skip"; //$NON-NLS-1$

	/** Environment variable that is defined in SARL for skipping inferring of JVM elements.
	 *
	 * @since 0.12
	 */
	public static final String SARL_JVMINFERRER_SKIP_NAME = "sarl.jvminferrer.skip"; //$NON-NLS-1$

	/** Environment variable that is defined in SARL for skipping cleaning stage.
	 *
	 * @since 0.12
	 */
	public static final String SARL_CLEAN_SKIP_NAME = "sarl.clean.skip"; //$NON-NLS-1$

	/** The tool that permits to access to Maven features.
	 */
	protected MavenHelper mavenHelper;

	/**
	 * The current Maven session.
	 */
	@Parameter(defaultValue = "${session}", required = true, readonly = true)
	protected MavenSession session;

	/**
	 * The Build PluginManager component.
	 */
	@Inject
	private BuildPluginManager buildPluginManager;

	@Inject
	private RepositorySystem repositorySystem;

	@Inject
	private ResolutionErrorHandler resolutionErrorHandler;

	/** The directory in which the Java code files are generated from the standard SARL code files.
	 */
	@Parameter(defaultValue = SARLConfig.FOLDER_SOURCE_GENERATED)
	private File output;

	/** The directory in which the standard SARL code files are located.
	 */
	@Parameter(defaultValue = SARLConfig.FOLDER_SOURCE_SARL)
	private File input;

	/** The directory in which the Java code files are generated from the test SARL code files.
	 */
	@Parameter(defaultValue = SARLConfig.FOLDER_TEST_SOURCE_GENERATED)
	private File testOutput;

	/** The directory in which the test SARL code files are located.
	 */
	@Parameter(defaultValue = SARLConfig.FOLDER_TEST_SOURCE_SARL)
	private File testInput;

	/** The directory in which the integration test SARL code files are located.
	 * @since 0.13
	 */
	@Parameter(defaultValue = SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL)
	private File integrationTestInput;

	/** Skip the execution of the mojo.
	 * @since 0.11
	 */
	@Parameter(defaultValue = "false")
	private boolean skip;

	/** Instance logger */
    private Logger logger;

    /** Replies the logger for this mojo.
     * This function is a replacement for {@link #getLog()}.
     *
     * @return the logger.
     * @since 0.13
     */
    protected Logger getLogger() {
    	if (this.logger == null) {
    		this.logger = LoggerFactory.getLogger(getClass());
    	}
    	return this.logger;
    }
	
	/** Replies if the execution of the mojo should be skipped or not.
	 * This function checks the configuration tag "skip".
	 *
	 * @return {@code true} if the mojo should be skipped.
	 * @since 0.11
	 */
	protected boolean isSkipped() {
		return this.skip;
	}

	@Override
	public final void execute() throws MojoExecutionException, MojoFailureException {
		if (isSkipped()) {
			getLogger().info(Messages.AbstractSarlMojo_5);
			return;
		}
		try {
			this.mavenHelper = new MavenHelper(this.session, this.buildPluginManager, this.repositorySystem,
					this.resolutionErrorHandler, getLogger());
			ensureDefaultParameterValues();
			prepareExecution();
			executeMojo();
		} catch (MojoExecutionException | MojoFailureException e) {
			throw e;
		} catch (Throwable e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
	}

	/** Ensure the mojo parameters have at least their default values.
	 */
	protected void ensureDefaultParameterValues() {
		//
	}

	/** Prepare the execution of the Mojo.
	 * @throws MojoExecutionException on failure.
	 */
	protected void prepareExecution() throws MojoExecutionException {
		//
	}

	/** Create a file from a unix-like representation of the filename.
	 *
	 * @param filename the unix-like filename.
	 * @return the file.
	 */
	protected static File unix2os(String filename) {
		File file = null;
		for (final var base : filename.split(Pattern.quote("/"))) { //$NON-NLS-1$
			if (file == null) {
				file = new File(base);
			} else {
				file = new File(file, base);
			}
		}
		return file;
	}

	/** Make absolute the given filename, relatively to the project's folder.
	 *
	 * @param file the file to convert.
	 * @return the absolute filename.
	 */
	protected File makeAbsolute(File file) {
		if (!file.isAbsolute()) {
			final var basedir = this.mavenHelper.getSession().getCurrentProject().getBasedir();
			return new File(basedir, file.getPath()).getAbsoluteFile();
		}
		return file;
	}

	/** Replies the input folder.
	 *
	 * @return the input folder.
	 */
	protected File getInput() {
		return makeAbsolute((this.input == null) ? unix2os(SARLConfig.FOLDER_SOURCE_SARL) : this.input);
	}

	/** Replies the output folder.
	 *
	 * @return the output folder.
	 */
	protected File getOutput() {
		return this.output == null ? getDefaultOutput() : makeAbsolute(this.output);
	}

	/** Replies the default output folder.
	 *
	 * @return the default output folder.
	 */
	protected File getDefaultOutput() {
		return makeAbsolute(unix2os(SARLConfig.FOLDER_SOURCE_GENERATED));
	}

	/** Replies the default output folder for tests.
	 *
	 * @return the default output folder for tests.
	 * @since 0.8
	 */
	protected File getDefaultTestOutput() {
		return makeAbsolute(unix2os(SARLConfig.FOLDER_TEST_SOURCE_GENERATED));
	}

	/** Replies the test input folder.
	 *
	 * @return the test input folder.
	 */
	protected File getTestInput() {
		return makeAbsolute((this.testInput == null) ? unix2os(SARLConfig.FOLDER_TEST_SOURCE_SARL) : this.testInput);
	}

	/** Replies the integration test input folder.
	 *
	 * @return the integration test input folder.
	 * @since 0.13
	 */
	protected File getIntegrationTestInput() {
		return makeAbsolute((this.integrationTestInput == null) ? unix2os(SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL) : this.integrationTestInput);
	}

	/** Replies the test output folder.
	 *
	 * @return the test output folder.
	 */
	protected File getTestOutput() {
		return makeAbsolute((this.testOutput == null) ? unix2os(SARLConfig.FOLDER_TEST_SOURCE_GENERATED) : this.testOutput);
	}

	/** Execute the mojo.
	 *
	 * @throws MojoExecutionException if an unexpected problem occurs. Throwing this
	 *     exception causes a "BUILD ERROR" message to be displayed.
	 * @throws MojoFailureException if an expected problem (such as a compilation failure)
	 *     occurs. Throwing this exception causes a "BUILD FAILURE" message to be displayed.
	 * @throws DependencyResolutionRequiredException if a dependency cannot be resolved.
	 */
	protected abstract void executeMojo() throws MojoExecutionException, MojoFailureException, DependencyResolutionRequiredException;

	/** Execute another MOJO.
	 *
	 * @param groupId identifier of the MOJO plugin group.
	 * @param artifactId identifier of the MOJO plugin artifact.
	 * @param version version of the MOJO plugin version.
	 * @param goal the goal to run.
	 * @param configuration the XML code for the configuration.
	 * @param dependencies the dependencies of the plugin.
	 * @throws MojoExecutionException when cannot run the MOJO.
	 * @throws MojoFailureException when the build failed.
	 */
	protected void executeMojo(
			String groupId, String artifactId,
			String version, String goal,
			String configuration,
			Dependency... dependencies) throws MojoExecutionException, MojoFailureException {
		final var plugin = new Plugin();
		plugin.setArtifactId(artifactId);
		plugin.setGroupId(groupId);
		plugin.setVersion(version);
		plugin.setDependencies(Arrays.asList(dependencies));

		getLogger().debug(MessageFormat.format(Messages.AbstractSarlMojo_0, plugin.getId()));

		final var pluginDescriptor = this.mavenHelper.loadPlugin(plugin);
		if (pluginDescriptor == null) {
			throw new MojoExecutionException(MessageFormat.format(Messages.AbstractSarlMojo_1, plugin.getId()));
		}
		final var mojoDescriptor = pluginDescriptor.getMojo(goal);
		if (mojoDescriptor == null) {
			throw new MojoExecutionException(MessageFormat.format(Messages.AbstractSarlMojo_2, goal));
		}

		final Xpp3Dom mojoXml;
		try {
			mojoXml = this.mavenHelper.toXpp3Dom(mojoDescriptor.getMojoConfiguration());
		} catch (PlexusConfigurationException e1) {
			throw new MojoExecutionException(e1.getLocalizedMessage(), e1);
		}
		var configurationXml = this.mavenHelper.toXpp3Dom(configuration, getLogger());
		if (configurationXml != null) {
			configurationXml = Xpp3DomUtils.mergeXpp3Dom(
					configurationXml,
					mojoXml);
		} else {
			configurationXml = mojoXml;
		}

		getLogger().debug(MessageFormat.format(Messages.AbstractSarlMojo_3, plugin.getId(), configurationXml.toString()));

		final MojoExecution execution = new MojoExecution(mojoDescriptor, configurationXml);

		this.mavenHelper.executeMojo(execution);
	}

	/** Extract the dependencies that are declared for a Maven plugin.
	 * This function reads the list of the dependencies in the configuration
	 * resource file with {@link MavenHelper#getConfig(String)}.
	 * The key given to {@link MavenHelper#getConfig(String)} is
	 * {@code &lt;configurationKeyPrefix&gt;.dependencies}.
	 *
	 * @param configurationKeyPrefix the string that is the prefix in the configuration file.
	 * @return the list of the dependencies.
	 * @throws MojoExecutionException if something cannot be done when extracting the dependencies.
	 */
	protected Dependency[] getDependenciesFor(String configurationKeyPrefix) throws MojoExecutionException {
		final var dependencies = new ArrayList<Dependency>();
		final var pattern = Pattern.compile(
				"^[ \t\n\r]*([^: \t\n\t]+)[ \t\n\r]*:[ \t\n\r]*([^: \t\n\t]+)[ \t\n\r]*$"); //$NON-NLS-1$
		final var rawDependencies = this.mavenHelper.getConfig(configurationKeyPrefix + ".dependencies"); //$NON-NLS-1$

		final var pomDependencies = this.mavenHelper.getPluginDependencies();

		for (final var dependencyId : rawDependencies.split("\\s*[;|,]+\\s*")) { //$NON-NLS-1$
			final var matcher = pattern.matcher(dependencyId);
			if (matcher != null && matcher.matches()) {
				final var dependencyGroupId = matcher.group(1);
				final var dependencyArtifactId = matcher.group(2);
				final var dependencyKey = ArtifactUtils.versionlessKey(dependencyGroupId, dependencyArtifactId);
				final var dependencyObject = pomDependencies.get(dependencyKey);
				if (dependencyObject == null) {
					throw new MojoExecutionException(MessageFormat.format(
							Messages.AbstractSarlMojo_4, dependencyKey));
				}
				dependencies.add(dependencyObject);
			}
		}

		final var dependencyArray = new Dependency[dependencies.size()];
		dependencies.toArray(dependencyArray);
		return dependencyArray;
	}

	/** Put the string representation of the properties of this object into the given buffer.
	 *
	 * @param buffer the buffer.
	 */
	protected void buildPropertyString(StringBuilder buffer) {
		buffer.append("input = ").append(this.input).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("output = ").append(this.output).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("testInput = ").append(this.testInput).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("integrationTestInput = ").append(this.integrationTestInput).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
		buffer.append("testOutput = ").append(this.testOutput).append("\n"); //$NON-NLS-1$//$NON-NLS-2$
	}

	/** Replies the classpath of the Maven platform. Assuming that it is not the module path.
	 *
	 * @return the current classpath.
	 * @since 0.13
	 */
	protected List<URL> getMavenPlatformClassPath() {
		final var cl = getClass().getClassLoader();
		if (cl instanceof URLClassLoader ucl) {
			return Arrays.asList(ucl.getURLs());
		}
		final var paths = System.getProperty("java.class.path").split(Pattern.quote(File.pathSeparator)); //$NON-NLS-1$
		final var files = new ArrayList<URL>(paths.length);
		for (final var path : paths) {
			files.add(FileSystem.convertStringToURL(path, false));
		}
		return files;
	}

}
