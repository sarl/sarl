/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.m2e.config;

import java.io.File;
import java.text.MessageFormat;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;

import com.google.common.base.Strings;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.ArtifactUtils;
import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.eclipse.aether.graph.DependencyNode;
import org.eclipse.aether.graph.DependencyVisitor;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.internal.M2EUtils;
import org.eclipse.m2e.core.lifecyclemapping.model.IPluginExecutionMetadata;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
import org.eclipse.m2e.core.project.MavenProjectUtils;
import org.eclipse.m2e.core.project.configurator.AbstractBuildParticipant;
import org.eclipse.m2e.core.project.configurator.AbstractBuildParticipant2;
import org.eclipse.m2e.core.project.configurator.AbstractProjectConfigurator;
import org.eclipse.m2e.core.project.configurator.ProjectConfigurationRequest;
import org.eclipse.m2e.jdt.IClasspathDescriptor;
import org.eclipse.m2e.jdt.IClasspathEntryDescriptor;
import org.eclipse.m2e.jdt.IJavaProjectConfigurator;
import org.eclipse.m2e.jdt.internal.ClasspathDescriptor;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;
import org.sonatype.plexus.build.incremental.BuildContext;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.util.Utilities;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.ui.preferences.SARLPreferences;
import io.sarl.m2e.utils.M2EUtilities;

/** Project configuration for the M2E.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectConfigurator extends AbstractProjectConfigurator implements IJavaProjectConfigurator {

	private static final String SARL_LANG_BUNDLE_NAME = "io.sarl.lang.core"; //$NON-NLS-1$

	private static final String SARL_GROUP_ID = "io.sarl.lang"; //$NON-NLS-1$

	private static final String SARL_ARTIFACT_ID = "io.sarl.lang.core"; //$NON-NLS-1$

	private static final String SARL_MAVENLIB_GROUP_ID = "io.sarl.maven"; //$NON-NLS-1$

	private static final String SARL_MAVENLIB_ARTIFACT_ID = "io.sarl.maven.sdk"; //$NON-NLS-1$

	private static final String SARL_PLUGIN_GROUP_ID = "io.sarl.maven"; //$NON-NLS-1$

	private static final String SARL_PLUGIN_ARTIFACT_ID = "sarl-maven-plugin"; //$NON-NLS-1$

	/** Invoked to add the preferences dedicated to SARL, JRE, etc.
	 *
	 * @param facade the Maven face.
	 * @param config the configuration.
	 * @param monitor the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings("static-method")
	protected void addPreferences(
			IMavenProjectFacade facade, SARLConfiguration config,
			IProgressMonitor monitor) throws CoreException {
		final IPath outputPath = makeProjectRelativePath(facade, config.getOutput());
		// Set the SARL preferences
		SARLPreferences.setSpecificSARLConfigurationFor(
				facade.getProject(), outputPath);
	}

	private static IPath makeFullPath(IMavenProjectFacade facade, File file) {
		assert file != null;
		final IProject project = facade.getProject();
		final IPath path;
		if (!file.isAbsolute()) {
			path =  Path.fromOSString(file.getPath());
		} else {
			path = MavenProjectUtils.getProjectRelativePath(project, file.getAbsolutePath());
		}
		return project.getFullPath().append(path);
	}

	private static IPath makeProjectRelativePath(IMavenProjectFacade facade, File file) {
		assert file != null;
		final IProject project = facade.getProject();
		if (!file.isAbsolute()) {
			return Path.fromOSString(file.getPath());
		}
		return MavenProjectUtils.getProjectRelativePath(project, file.getAbsolutePath());
	}

	private static IFolder ensureFolderExists(IMavenProjectFacade facade, IPath path, boolean derived,
			IProgressMonitor monitor) throws CoreException {
		final IFolder folder = facade.getProject().getFolder(path.makeRelativeTo(facade.getProject().getFullPath()));
		assert folder != null;
		if (!folder.exists()) {
			M2EUtils.createFolder(folder, derived || folder.isDerived(), monitor);
		}
		return folder;
	}

	/** Invoked to add the source folders.
	 *
	 * @param facade the facade of the Maven project.
	 * @param config the configuration.
	 * @param classpath the project classpath.
	 * @param monitor the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings({"checkstyle:magicnumber", "checkstyle:npathcomplexity"})
	protected void addSourceFolders(
			IMavenProjectFacade facade, SARLConfiguration config,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {

		assertHasNature(facade.getProject(), SARLEclipseConfig.NATURE_ID);
		assertHasNature(facade.getProject(), SARLEclipseConfig.XTEXT_NATURE_ID);
		assertHasNature(facade.getProject(), JavaCore.NATURE_ID);

		final String encoding = config.getEncoding();

		final SubMonitor subMonitor = SubMonitor.convert(monitor, 4);

		//
		// Add the source folders
		//
		// Input folder, e.g. "src/main/sarl"
		final IPath inputPath = makeFullPath(facade, config.getInput());
		final IFolder inputFolder = ensureFolderExists(facade, inputPath, false, subMonitor);
		if (encoding != null && inputFolder != null && inputFolder.exists()) {
			inputFolder.setDefaultCharset(encoding, monitor);
		}
		IClasspathEntryDescriptor descriptor = classpath.addSourceEntry(
				inputPath,
				facade.getOutputLocation(),
				false);
		descriptor.setPomDerived(true);
		subMonitor.worked(1);

		// Input folder, e.g. "src/main/generated-sources/sarl"
		final IPath outputPath = makeFullPath(facade, config.getOutput());
		final IFolder outputFolder = ensureFolderExists(facade, outputPath, true, subMonitor);
		if (encoding != null && outputFolder != null && outputFolder.exists()) {
			outputFolder.setDefaultCharset(encoding, monitor);
		}
		descriptor = classpath.addSourceEntry(
				outputPath,
				facade.getOutputLocation(),
				false);
		descriptor.setPomDerived(true);
		descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
		subMonitor.worked(1);

		// Test input folder, e.g. "src/test/sarl"
		final IPath testInputPath = makeFullPath(facade, config.getTestInput());
		final IFolder testInputFolder = ensureFolderExists(facade, testInputPath, false, subMonitor);
		if (encoding != null && testInputFolder != null && testInputFolder.exists()) {
			testInputFolder.setDefaultCharset(encoding, monitor);
		}
		descriptor = classpath.addSourceEntry(
				testInputPath,
				facade.getTestOutputLocation(),
				true);
		descriptor.setPomDerived(true);
		descriptor.setClasspathAttribute(IClasspathAttribute.TEST, Boolean.TRUE.toString());
		subMonitor.worked(1);

		// Test input folder, e.g. "src/test/generated-sources/sarl"
		final IPath testOutputPath = makeFullPath(facade, config.getTestOutput());
		final IFolder testOutputFolder = ensureFolderExists(facade, testOutputPath, true, subMonitor);
		if (encoding != null && testOutputFolder != null && testOutputFolder.exists()) {
			testOutputFolder.setDefaultCharset(encoding, monitor);
		}
		descriptor = classpath.addSourceEntry(
				testOutputPath,
				facade.getTestOutputLocation(),
				true);
		descriptor.setPomDerived(true);
		descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
		descriptor.setClasspathAttribute(IClasspathAttribute.TEST, Boolean.TRUE.toString());
		subMonitor.done();
	}

	/** Replies the configuration value.
	 *
	 * @param <T> - the expected type.
	 * @param project the project.
	 * @param parameter the parameter name.
	 * @param asType the expected type.
	 * @param mojoExecution the mojo execution.
	 * @param monitor the monitor.
	 * @param defaultValue the default value.
	 * @return the value of the parameter.
	 * @throws CoreException if cannot read the value.
	 */
	protected <T> T getParameterValue(MavenProject project, String parameter, Class<T> asType,
			MojoExecution mojoExecution, IProgressMonitor monitor, T defaultValue) throws CoreException {
		T value = getParameterValue(project, parameter, asType, mojoExecution, monitor);
		if (value == null) {
			value = defaultValue;
		}
		return value;
	}

	/** Read the SARL configuration.
	 *
	 * @param request the configuration request.
	 * @param monitor the monitor.
	 * @return the SARL configuration.
	 * @throws CoreException if something wrong appends.
	 */
	protected SARLConfiguration readConfiguration(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {

		SARLConfiguration initConfig = null;
		SARLConfiguration compileConfig = null;

		final List<MojoExecution> mojos = getMojoExecutions(request, monitor);
		for (final MojoExecution mojo : mojos) {
			final String goal = mojo.getGoal();
			switch (goal) {
			case "initialize": //$NON-NLS-1$
				initConfig = readInitializeConfiguration(request, mojo, monitor);
				break;
			case "compile": //$NON-NLS-1$
				compileConfig = readCompileConfiguration(request, mojo, monitor);
				break;
			default:
			}
		}

		if (compileConfig != null && initConfig != null) {
			compileConfig.setFrom(initConfig);
		}

		return compileConfig;
	}

	/** Read the configuration for the Initialize mojo.
	 *
	 * @param request the request.
	 * @param mojo the mojo execution.
	 * @param monitor the monitor.
	 * @return the configuration.
	 * @throws CoreException error in the eCore configuration.
	 */
	private SARLConfiguration readInitializeConfiguration(
			ProjectConfigurationRequest request, MojoExecution mojo, IProgressMonitor monitor)
					throws CoreException {
		final SARLConfiguration config = new SARLConfiguration();

		final MavenProject project = request.getMavenProject();

		final File input = getParameterValue(project, "input", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_SARL));
		final File output = getParameterValue(project, "output", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_GENERATED));
		final File binOutput = getParameterValue(project, "binOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_BIN));
		final File testInput = getParameterValue(project, "testInput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_SARL));
		final File testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_GENERATED));
		final File testBinOutput = getParameterValue(project, "testBinOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_BIN));

		config.setInput(input);
		config.setOutput(output);
		config.setBinOutput(binOutput);
		config.setTestInput(testInput);
		config.setTestOutput(testOutput);
		config.setTestBinOutput(testBinOutput);

		return config;
	}

	/** Read the configuration for the Compilation mojo.
	 *
	 * @param request the request.
	 * @param mojo the mojo execution.
	 * @param monitor the monitor.
	 * @return the configuration.
	 * @throws CoreException error in the eCore configuration.
	 */
	private SARLConfiguration readCompileConfiguration(
			ProjectConfigurationRequest request, MojoExecution mojo, IProgressMonitor monitor)
					throws CoreException {
		final SARLConfiguration config = new SARLConfiguration();

		final MavenProject project = request.getMavenProject();

		final File input = getParameterValue(project, "input", File.class, mojo, monitor); //$NON-NLS-1$
		final File output = getParameterValue(project, "output", File.class, mojo, monitor); //$NON-NLS-1$
		final File binOutput = getParameterValue(project, "binOutput", File.class, mojo, monitor); //$NON-NLS-1$
		final File testInput = getParameterValue(project, "testInput", File.class, mojo, monitor); //$NON-NLS-1$
		final File testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor); //$NON-NLS-1$
		final File testBinOutput = getParameterValue(project, "testBinOutput", File.class, mojo, monitor); //$NON-NLS-1$

		config.setInput(input);
		config.setOutput(output);
		config.setBinOutput(binOutput);
		config.setTestInput(testInput);
		config.setTestOutput(testOutput);
		config.setTestBinOutput(testBinOutput);

		final String inputCompliance = getParameterValue(project, "source", String.class, mojo, monitor); //$NON-NLS-1$
		final String outputCompliance = getParameterValue(project, "target", String.class, mojo, monitor); //$NON-NLS-1$

		config.setInputCompliance(inputCompliance);
		config.setOutputCompliance(outputCompliance);

		final String encoding = getParameterValue(project, "encoding", String.class, mojo, monitor); //$NON-NLS-1$
		config.setEncoding(encoding);

		return config;
	}

	/** Remove any reference to the SARL libraries from the given classpath.
	 *
	 * @param classpath the classpath to update.
	 */
	@SuppressWarnings("static-method")
	protected void removeSarlLibraries(IClasspathDescriptor classpath) {
		classpath.removeEntry(SARLClasspathContainerInitializer.CONTAINER_ID);
	}

	/** Add the SARL libraries into the given classpath.
	 *
	 * @param classpath the classpath to update.
	 */
	@SuppressWarnings("static-method")
	protected void addSarlLibraries(IClasspathDescriptor classpath) {
		final IClasspathEntry entry = JavaCore.newContainerEntry(SARLClasspathContainerInitializer.CONTAINER_ID);
		classpath.addEntry(entry);
	}

	private static void setVersion(Properties props, String propName, String value, String minValue) {
		final String currentVersion = props.getProperty(propName);
		String newVersion = value;

		if (M2EUtilities.compareOsgiVersions(currentVersion, newVersion) > 0) {
			newVersion = currentVersion;
		}

		if (M2EUtilities.compareOsgiVersions(newVersion, minValue) < 0) {
			props.setProperty(propName, minValue);
		} else {
			props.setProperty(propName, newVersion);
		}
	}

	private static void forceMavenCompilerConfiguration(IMavenProjectFacade facade, SARLConfiguration config) {
		final Properties props = facade.getMavenProject().getProperties();
		setVersion(props, "maven.compiler.source", config.getInputCompliance(), //$NON-NLS-1$
				SARLVersion.MINIMAL_JDK_VERSION);
		setVersion(props, "maven.compiler.target", config.getOutputCompliance(), //$NON-NLS-1$
				SARLVersion.MINIMAL_JDK_VERSION);
		final String encoding = config.getEncoding();
		if (encoding != null && !encoding.isEmpty()) {
			props.setProperty("maven.compiler.encoding", encoding); //$NON-NLS-1$
		}
	}

	@Override
	public void configure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		final SubMonitor subMonitor = SubMonitor.convert(monitor, 3);
		final SARLConfiguration config = readConfiguration(request, subMonitor.newChild(1));
		forceMavenCompilerConfiguration(request.getMavenProjectFacade(), config);
		subMonitor.worked(1);
		io.sarl.eclipse.natures.SARLProjectConfigurator.addSarlNatures(
				request.getProject(),
				subMonitor.newChild(1));
	}

	@Override
	public void unconfigure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		final IJavaProject javaProject = JavaCore.create(request.getProject());
		final IClasspathDescriptor classpath = new ClasspathDescriptor(javaProject);
		addSarlLibraries(classpath);
	}

	@Override
	public void configureClasspath(IMavenProjectFacade facade,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {
		//
	}

	@Override
	public void configureRawClasspath(ProjectConfigurationRequest request,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {
		final SubMonitor subm = SubMonitor.convert(monitor, 4);
		final IMavenProjectFacade facade = request.getMavenProjectFacade();
		final SARLConfiguration config = readConfiguration(request, subm.newChild(1));
		subm.worked(1);
		removeSarlLibraries(classpath);
		subm.worked(2);
		addSourceFolders(facade, config, classpath, subm.newChild(1));
		subm.worked(1);
		addPreferences(facade, config, subm.newChild(1));
		subm.worked(1);
	}

	@Override
	public AbstractBuildParticipant getBuildParticipant(
			IMavenProjectFacade projectFacade, MojoExecution execution,
			IPluginExecutionMetadata executionMetadata) {
		return new BuildParticipant();
	}

	/** Build participant for detecting invalid versions of SARL components.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	protected static class BuildParticipant extends AbstractBuildParticipant2 {

		private static final int NSTEPS = 4;

		/** Construct a build participant.
		 */
		public BuildParticipant() {
			//
		}

		@Override
		public Set<IProject> build(int kind, IProgressMonitor monitor) throws Exception {
			if (kind == AbstractBuildParticipant.AUTO_BUILD || kind == AbstractBuildParticipant.FULL_BUILD) {
				final SubMonitor subm = SubMonitor.convert(monitor, Messages.SARLProjectConfigurator_7, NSTEPS);
				getBuildContext().removeMessages(getMavenProjectFacade().getPomFile());
				subm.worked(1);
				validateSARLCompilerPlugin();
				subm.worked(2);
				validateSARLLibraryVersion();
				subm.worked(3);
				validateSARLDependenciesVersions(subm.newChild(1));
				subm.worked(NSTEPS);
			}
			return null;
		}

		private Bundle validateSARLVersion(String groupId, String artifactId, String artifactVersion) {
			final Bundle bundle = Platform.getBundle(SARL_LANG_BUNDLE_NAME);
			if (bundle == null) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						MessageFormat.format(Messages.SARLProjectConfigurator_0, SARL_LANG_BUNDLE_NAME),
						BuildContext.SEVERITY_ERROR,
						null);
				return bundle;
			}

			final Version bundleVersion = bundle.getVersion();
			if (bundleVersion == null) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						MessageFormat.format(Messages.SARLProjectConfigurator_1, SARL_LANG_BUNDLE_NAME),
						BuildContext.SEVERITY_ERROR,
						null);
				return bundle;
			}

			final Version minVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor(), 0);
			final Version maxVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor() + 1, 0);
			assert minVersion != null && maxVersion != null;

			final Version mvnVersion = M2EUtilities.parseMavenVersion(artifactVersion);
			final int compare = Utilities.compareVersionToRange(mvnVersion, minVersion, maxVersion);
			if (compare < 0) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						MessageFormat.format(Messages.SARLProjectConfigurator_2,
								groupId, artifactId, artifactVersion, minVersion.toString()),
						BuildContext.SEVERITY_ERROR,
						null);
			} else if (compare > 0) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						MessageFormat.format(Messages.SARLProjectConfigurator_3,
								groupId, artifactId, artifactVersion, maxVersion.toString()),
						BuildContext.SEVERITY_ERROR,
						null);
			}
			return bundle;
		}

		/** Validate the version of the SARL library in the dependencies.
		 *
		 * @throws CoreException if internal error occurs.
		 */
		protected void validateSARLLibraryVersion() throws CoreException {
			final Map<String, Artifact> artifacts = getMavenProjectFacade().getMavenProject().getArtifactMap();
			final Artifact artifact = artifacts.get(ArtifactUtils.versionlessKey(SARL_GROUP_ID, SARL_ARTIFACT_ID));
			if (artifact != null) {
				validateSARLVersion(SARL_GROUP_ID, SARL_ARTIFACT_ID, artifact.getVersion());
			} else {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						Messages.SARLProjectConfigurator_6,
						BuildContext.SEVERITY_ERROR,
						null);
			}
		}

		/** Validate the version of the SARL compiler in the Maven configuration.
		 *
		 * @return the SARL bundle.
		 * @throws CoreException if internal error occurs.
		 */
		protected Bundle validateSARLCompilerPlugin() throws CoreException {
			final Map<String, Artifact> plugins = getMavenProjectFacade().getMavenProject().getPluginArtifactMap();
			final Artifact pluginArtifact = plugins.get(ArtifactUtils.versionlessKey(SARL_PLUGIN_GROUP_ID,
					SARL_PLUGIN_ARTIFACT_ID));
			if (pluginArtifact == null) {
				getBuildContext().addMessage(
						getMavenProjectFacade().getPomFile(),
						-1, -1,
						Messages.SARLProjectConfigurator_5,
						BuildContext.SEVERITY_ERROR,
						null);
			} else {
				final String version = pluginArtifact.getVersion();
				if (Strings.isNullOrEmpty(version)) {
					getBuildContext().addMessage(
							getMavenProjectFacade().getPomFile(),
							-1, -1,
							Messages.SARLProjectConfigurator_5,
							BuildContext.SEVERITY_ERROR,
							null);
				} else {
					return validateSARLVersion(
							SARL_PLUGIN_GROUP_ID, SARL_PLUGIN_ARTIFACT_ID,
							version);
				}
			}
			return null;
		}

		/** Validate the versions of the libraries that are in the project dependencies have compatible versions
		 * with the specific dependencies of the SARL library.
		 *
		 * <p>The nearest-win strategy of the dependency resolver may select invalid version for artifacts
		 * that are used by the SARL libraries.
		 *
		 * @param monitor the progress monitor.
		 * @throws CoreException if internal error occurs.
		 */
		protected void validateSARLDependenciesVersions(IProgressMonitor monitor) throws CoreException {
			final SubMonitor subm = SubMonitor.convert(monitor, 3);

			final Map<String, String> neededArtifactVersions = new TreeMap<>();
			final IMavenProjectFacade facade = getMavenProjectFacade();
			final DependencyNode root = MavenPlugin.getMavenModelManager().readDependencyTree(
					facade, facade.getMavenProject(),
					Artifact.SCOPE_COMPILE,
					subm.newChild(1));
			final DependencyNode[] sarlNode = new DependencyNode[] {null};
			root.accept(new DependencyVisitor() {
				@Override
				public boolean visitLeave(DependencyNode node) {
					if (sarlNode[0] == null
						&& node.getDependency() != null
						&& Objects.equals(node.getDependency().getArtifact().getGroupId(), SARL_MAVENLIB_GROUP_ID)
						&& Objects.equals(node.getDependency().getArtifact().getArtifactId(), SARL_MAVENLIB_ARTIFACT_ID)) {
						sarlNode[0] = node;
						return false;
					}
					return true;
				}

				@Override
				public boolean visitEnter(DependencyNode node) {
					return sarlNode[0] == null;
				}
			});

			subm.worked(1);

			if (sarlNode[0] != null) {
				sarlNode[0].accept(new DependencyVisitor() {
					@Override
					public boolean visitLeave(DependencyNode node) {
						if (node.getDependency() != null) {
							final String grId = node.getDependency().getArtifact().getGroupId();
							final String arId = node.getDependency().getArtifact().getArtifactId();
							final String key = ArtifactUtils.versionlessKey(grId, arId);
							final String vers = neededArtifactVersions.get(key);
							if (vers == null
								|| M2EUtilities.compareMavenVersions(vers, node.getVersion().toString()) < 0) {
								neededArtifactVersions.put(key, node.getVersion().toString());
							}
						}
						return true;
					}

					@Override
					public boolean visitEnter(DependencyNode node) {
						return true;
					}
				});
			}

			subm.worked(2);
			final SubMonitor subm2 = SubMonitor.convert(subm, neededArtifactVersions.size());
			int i = 0;
			final Map<String, Artifact> artifacts = getMavenProjectFacade().getMavenProject().getArtifactMap();

			for (final Entry<String, String> neededDependency : neededArtifactVersions.entrySet()) {
				final Artifact artifact = artifacts.get(neededDependency.getKey());
				if (artifact != null) {
					final int cmp = M2EUtilities.compareMavenVersions(neededDependency.getValue(), artifact.getVersion());
					if (cmp > 1) {
						getBuildContext().addMessage(
								getMavenProjectFacade().getPomFile(),
								-1, -1,
								MessageFormat.format(
										Messages.SARLProjectConfigurator_8,
										artifact.getGroupId(),
										artifact.getArtifactId(),
										artifact.getVersion(),
										neededDependency.getValue()),
								BuildContext.SEVERITY_ERROR,
								null);
					}
				}
				subm2.worked(i);
				++i;
			}

			subm.worked(3);
		}
	}

}
