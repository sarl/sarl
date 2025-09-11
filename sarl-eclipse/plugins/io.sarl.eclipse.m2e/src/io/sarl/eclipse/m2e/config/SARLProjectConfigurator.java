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

package io.sarl.eclipse.m2e.config;

import static io.sarl.eclipse.natures.SARLProjectConfigurator.addSarlNatures;

import java.io.File;
import java.util.Arrays;
import java.util.Properties;

import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.internal.M2EUtils;
import org.eclipse.m2e.core.lifecyclemapping.model.IPluginExecutionMetadata;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
import org.eclipse.m2e.core.project.MavenProjectUtils;
import org.eclipse.m2e.core.project.configurator.AbstractBuildParticipant;
import org.eclipse.m2e.core.project.configurator.AbstractProjectConfigurator;
import org.eclipse.m2e.core.project.configurator.ProjectConfigurationRequest;
import org.eclipse.m2e.jdt.IClasspathDescriptor;
import org.eclipse.m2e.jdt.IJavaProjectConfigurator;
import org.eclipse.m2e.jdt.MavenJdtPlugin;
import org.eclipse.m2e.jdt.internal.ClasspathDescriptor;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.eclipse.m2e.build.BuildParticipant;
import io.sarl.eclipse.m2e.utils.M2EUtilities;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.ui.preferences.SARLPreferences;

/** Project configuration for the M2E.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse.m2e 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.m2e
 */
@SuppressWarnings("restriction")
public class SARLProjectConfigurator extends AbstractProjectConfigurator implements IJavaProjectConfigurator {

	private static final String ECLIPSE_PLUGIN_PACKAGING = "eclipse-plugin"; //$NON-NLS-1$

	/** Invoked to add the preferences dedicated to SARL, JRE, etc.
	 *
	 * @param facade the Maven face.
	 * @param config the configuration.
	 * @param addTestFolders indicates if the test folders should be considered.
	 * @param monitor the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings("static-method")
	protected void addPreferences(
			IMavenProjectFacade facade, SARLConfiguration config,
			boolean addTestFolders, IProgressMonitor monitor) throws CoreException {
		final var outputPath = makeProjectRelativePath(facade, config.getOutput());
		final var testOutputPath = addTestFolders ? makeProjectRelativePath(facade, config.getTestOutput()) : null;
		// Set the SARL preferences
		SARLPreferences.setSpecificSARLConfigurationFor(
				facade.getProject(),
				outputPath,
				testOutputPath);
	}

	private static IPath makeFullPath(IMavenProjectFacade facade, File file) {
		assert file != null;
		final var project = facade.getProject();
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
		final var project = facade.getProject();
		if (!file.isAbsolute()) {
			return Path.fromOSString(file.getPath());
		}
		return MavenProjectUtils.getProjectRelativePath(project, file.getAbsolutePath());
	}

	private static IFolder ensureFolderExists(IMavenProjectFacade facade, IPath path, boolean derived,
			IProgressMonitor monitor) throws CoreException {
		final var folder = facade.getProject().getFolder(path.makeRelativeTo(facade.getProject().getFullPath()));
		assert folder != null;
		if (!folder.exists()) {
			M2EUtils.createFolder(folder, derived || folder.isDerived(), monitor);
		}
		return folder;
	}

	/** Invoked to remove the source folder from the classpath.
	 *
	 * @param path the path to remove.
	 * @param classpath the project classpath.
	 * @param monitor the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings("static-method")
	protected void removeSourceFolder(IPath path, IClasspathDescriptor classpath, IProgressMonitor monitor) throws CoreException {
		final var subMonitor = SubMonitor.convert(monitor, 3);

		if (path == null) {
			subMonitor.done();
			return;
		}

		final var entries = classpath.getEntries();
		subMonitor.worked(1);

		final var subMonitor0 = SubMonitor.convert(subMonitor, entries.length);

		for (final var entry : entries) {
			final var type = entry.getEntryKind();
			if (type == IClasspathEntry.CPE_SOURCE && path.equals(entry.getPath())) {
				classpath.removeEntry(entry.getPath());
				subMonitor0.done();
				subMonitor.done();
				return;
			}
			subMonitor0.worked(1);
		}
		subMonitor0.done();
		subMonitor.done();
	}

	/** Invoked to add the source folders.
	 *
	 * @param facade the facade of the Maven project.
	 * @param config the configuration.
	 * @param classpath the project classpath.
	 * @param addTestFolders indicate if the test folders must be added into the classpath.
	 * @param monitor the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	protected void addSourceFolders(
			IMavenProjectFacade facade, SARLConfiguration config,
			IClasspathDescriptor classpath,
			boolean addTestFolders,
			IProgressMonitor monitor) throws CoreException {
		assertHasNature(facade.getProject(), SARLEclipseConfig.NATURE_ID);
		assertHasNature(facade.getProject(), SARLEclipseConfig.XTEXT_NATURE_ID);
		assertHasNature(facade.getProject(), JavaCore.NATURE_ID);

		final var subMonitor = SubMonitor.convert(monitor, 8);
		final var encoding = config.getEncoding();

		//
		// Add the source folders
		//
		// Input folder, e.g. "src/main/sarl"
		final var inputPath = makeFullPath(facade, config.getInput());
		removeSourceFolder(inputPath, classpath, subMonitor.newChild(1));
		final var inputFolder = ensureFolderExists(facade, inputPath, false, subMonitor);
		if (encoding != null && inputFolder != null && inputFolder.exists()) {
			inputFolder.setDefaultCharset(encoding, monitor);
		}
		// Remove any previous definition of the source entry
		classpath.touchEntry(inputPath);
		// Add the source entry
		var descriptor = classpath.addSourceEntry(
				inputPath,
				facade.getOutputLocation(),
				false);
		descriptor.setPomDerived(true);
		subMonitor.worked(1);

		// Input folder, e.g. "src/main/generated-sources/sarl"
		final var outputPath = makeFullPath(facade, config.getOutput());
		removeSourceFolder(outputPath, classpath, subMonitor.newChild(1));
		final var outputFolder = ensureFolderExists(facade, outputPath, true, subMonitor);
		if (encoding != null && outputFolder != null && outputFolder.exists()) {
			outputFolder.setDefaultCharset(encoding, monitor);
		}
		// Remove any previous definition of the source entry
		classpath.touchEntry(outputPath);
		// Add the source entry
		descriptor = classpath.addSourceEntry(
				outputPath,
				facade.getOutputLocation(),
				true);
		descriptor.setPomDerived(true);
		descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
		subMonitor.worked(1);

		if (addTestFolders) {
			// Test input folder, e.g. "src/test/sarl"
			final var testInputPath = makeFullPath(facade, config.getTestInput());
			removeSourceFolder(testInputPath, classpath, subMonitor.newChild(1));
			final var testInputFolder = ensureFolderExists(facade, testInputPath, false, subMonitor);
			if (encoding != null && testInputFolder != null && testInputFolder.exists()) {
				testInputFolder.setDefaultCharset(encoding, monitor);
			}
			// Remove any previous definition of the source entry
			classpath.touchEntry(testInputPath);
			// Add the source entry
			descriptor = classpath.addSourceEntry(
					testInputPath,
					facade.getTestOutputLocation(),
					false);
			descriptor.setPomDerived(true);
			descriptor.setClasspathAttribute(IClasspathAttribute.TEST, Boolean.TRUE.toString());
			subMonitor.worked(1);
		} else {
			subMonitor.worked(2);
		}

		if (addTestFolders) {
			// Test input folder, e.g. "src/it/sarl"
			final var integrationTestInputPath = makeFullPath(facade, config.getIntegrationTestInput());
			removeSourceFolder(integrationTestInputPath, classpath, subMonitor.newChild(1));
			final var integrationTestInputFolder = ensureFolderExists(facade, integrationTestInputPath, false, subMonitor);
			if (encoding != null && integrationTestInputFolder != null && integrationTestInputFolder.exists()) {
				integrationTestInputFolder.setDefaultCharset(encoding, monitor);
			}
			// Remove any previous definition of the source entry
			classpath.touchEntry(integrationTestInputPath);
			// Add the source entry
			descriptor = classpath.addSourceEntry(
					integrationTestInputPath,
					facade.getTestOutputLocation(),
					false);
			descriptor.setPomDerived(true);
			descriptor.setClasspathAttribute(IClasspathAttribute.TEST, Boolean.TRUE.toString());
			subMonitor.worked(1);
		} else {
			subMonitor.worked(2);
		}

		if (addTestFolders) {
			// Test input folder, e.g. "src/test/generated-sources/sarl"
			final var testOutputPath = makeFullPath(facade, config.getTestOutput());
			removeSourceFolder(testOutputPath, classpath, subMonitor.newChild(1));
			final var testOutputFolder = ensureFolderExists(facade, testOutputPath, true, subMonitor);
			if (encoding != null && testOutputFolder != null && testOutputFolder.exists()) {
				testOutputFolder.setDefaultCharset(encoding, monitor);
			}
			// Remove any previous definition of the source entry
			classpath.touchEntry(testOutputPath);
			// Add the source entry
			descriptor = classpath.addSourceEntry(
					testOutputPath,
					facade.getTestOutputLocation(),
					true);
			descriptor.setPomDerived(true);
			descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
			descriptor.setClasspathAttribute(IClasspathAttribute.TEST, Boolean.TRUE.toString());
		}
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
		var value = getParameterValue(project, parameter, asType, mojoExecution, monitor);
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

		final var mojos = getMojoExecutions(request, monitor);
		for (final var mojo : mojos) {
			final var goal = mojo.getGoal();
			switch (goal) {
			case "initialize": //$NON-NLS-1$
				initConfig = readInitializeConfiguration(request, mojo, monitor);
				break;
			case "compile": //$NON-NLS-1$
			case "testCompile": //$NON-NLS-1$
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
		final var config = new SARLConfiguration();

		final var project = request.mavenProject();

		final var input = getParameterValue(project, "input", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_SARL));
		final var output = getParameterValue(project, "output", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_GENERATED));
		final var binOutput = getParameterValue(project, "binOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_BIN));
		final var testInput = getParameterValue(project, "testInput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_SARL));
		final var integrationTestInput = getParameterValue(project, "integrationTestInput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL));
		final var testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_GENERATED));
		final var testBinOutput = getParameterValue(project, "testBinOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_BIN));

		config.setInput(input);
		config.setOutput(output);
		config.setBinOutput(binOutput);
		config.setTestInput(testInput);
		config.setIntegrationTestInput(integrationTestInput);
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
		final var config = new SARLConfiguration();

		final var project = request.mavenProject();

		final var input = getParameterValue(project, "input", File.class, mojo, monitor); //$NON-NLS-1$
		final var output = getParameterValue(project, "output", File.class, mojo, monitor); //$NON-NLS-1$
		final var binOutput = getParameterValue(project, "binOutput", File.class, mojo, monitor); //$NON-NLS-1$
		final var testInput = getParameterValue(project, "testInput", File.class, mojo, monitor); //$NON-NLS-1$
		final var integrationTestInput = getParameterValue(project, "integrationTestInput", File.class, mojo, monitor); //$NON-NLS-1$
		final var testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor); //$NON-NLS-1$
		final var testBinOutput = getParameterValue(project, "testBinOutput", File.class, mojo, monitor); //$NON-NLS-1$

		config.setInput(input);
		config.setOutput(output);
		config.setBinOutput(binOutput);
		config.setTestInput(testInput);
		config.setIntegrationTestInput(integrationTestInput);
		config.setTestOutput(testOutput);
		config.setTestBinOutput(testBinOutput);

		final var inputCompliance = getParameterValue(project, "source", String.class, mojo, monitor); //$NON-NLS-1$
		final var outputCompliance = getParameterValue(project, "target", String.class, mojo, monitor); //$NON-NLS-1$

		config.setInputCompliance(inputCompliance);
		config.setOutputCompliance(outputCompliance);

		final var encoding = getParameterValue(project, "encoding", String.class, mojo, monitor); //$NON-NLS-1$
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
		final var entry = JavaCore.newContainerEntry(SARLClasspathContainerInitializer.CONTAINER_ID);
		classpath.addEntry(entry);
	}

	private static void setVersion(Properties props, String propName, String value, String minValue, String incompatibleValue) {
		final var currentVersion = props.getProperty(propName);
		var newVersion = value;

		if (M2EUtilities.compareOsgiVersions(currentVersion, newVersion) > 0) {
			newVersion = currentVersion;
		}

		if (M2EUtilities.compareOsgiVersions(newVersion, minValue) < 0) {
			props.setProperty(propName, minValue);
		} else if (M2EUtilities.compareOsgiVersions(newVersion, incompatibleValue) >= 0) {
			props.setProperty(propName, M2EUtilities.getPreviousOsgiVersion(incompatibleValue));
		} else {
			props.setProperty(propName, newVersion);
		}
	}

	private static void forceMavenCompilerConfiguration(IMavenProjectFacade facade, SARLConfiguration config) {
		final var props = facade.getMavenProject().getProperties();
		setVersion(props, "maven.compiler.source", config.getInputCompliance(), //$NON-NLS-1$
				SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT,
				SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
		setVersion(props, "maven.compiler.target", config.getOutputCompliance(), //$NON-NLS-1$
				SARLVersion.MINIMAL_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT,
				SARLVersion.INCOMPATIBLE_JDK_VERSION_FOR_SARL_COMPILATION_ENVIRONMENT);
		final var encoding = config.getEncoding();
		if (encoding != null && !encoding.isEmpty()) {
			props.setProperty("maven.compiler.encoding", encoding); //$NON-NLS-1$
		}
	}

	/** Replies if the given project facade is pointing an Eclipe plugin.
	 *
	 * @param facade the maven project facade.
	 * @return {@code true} if the facade is for an Eclipse plugin.
	 * @since 0.11
	 */
	@SuppressWarnings("static-method")
	public boolean isEclipsePluginPackaging(IMavenProjectFacade facade) {
		return ECLIPSE_PLUGIN_PACKAGING.equalsIgnoreCase(facade.getPackaging());
	}

	@Override
	public void configure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		final var facade = request.mavenProjectFacade();

		final SubMonitor subMonitor;
		subMonitor = SubMonitor.convert(monitor, 5);
		try {
			final var project = request.mavenProjectFacade().getProject();

			final var config = readConfiguration(request, subMonitor.newChild(1));
			subMonitor.worked(1);
			forceMavenCompilerConfiguration(facade, config);
			subMonitor.worked(1);

			addSarlNatures(
					project,
					subMonitor.newChild(1));
			subMonitor.worked(1);

			final var javaProject = JavaCore.create(project);
			if (javaProject != null) {
				final var classpath = getMutableClasspath(javaProject);
				
				// Change the project configuration for SARL
				removeSarlLibraries(classpath);
				configureSarlProject(facade, config, classpath, true, subMonitor.newChild(1));
				
				// Save the new classpath of the project
				final var mavenProject = request.mavenProject();
				final var outputPath = makeFullPath(facade, new File(mavenProject.getBuild().getOutputDirectory()));
				final var classpathEntries = classpath.getEntries();
				sortClasspathEntries(classpathEntries);
				javaProject.setRawClasspath(classpathEntries, outputPath, subMonitor.newChild(1));
				MavenJdtPlugin.getDefault().getBuildpathManager().updateClasspath(project, subMonitor.newChild(1));
			}
			subMonitor.worked(3);
		} finally {
			subMonitor.done();
		}
	}

	private static int getFolderOrder(String name) {
		if (name.endsWith(SARLConfig.FOLDER_SOURCE_SARL) || name.endsWith(SARLConfig.FOLDER_TEST_SOURCE_SARL) || name.endsWith(SARLConfig.FOLDER_INTEGRATION_TEST_SOURCE_SARL)) {
			return 0;
		}
		if (name.endsWith(SARLConfig.FOLDER_RESOURCES) || name.endsWith(SARLConfig.FOLDER_TEST_RESOURCES)) {
			return 2;
		}
		if (name.endsWith(SARLConfig.FOLDER_SOURCE_GENERATED) || name.endsWith(SARLConfig.FOLDER_TEST_SOURCE_GENERATED)) {
			return 3;
		}
		return 1;
	}
	
	private static void sortClasspathEntries(IClasspathEntry[] entries) {
		Arrays.sort(entries, (a, b) -> {
			final var na = a.getPath().toPortableString();
			final var nb = b.getPath().toPortableString();
			final var nameCmp = na.compareTo(nb);
			if (nameCmp == 0) {
				return 0;
			}
			final var ta = a.getEntryKind();
			final var tb = b.getEntryKind();
			if (ta == tb) {
				if (ta == IClasspathEntry.CPE_SOURCE) {
					final var testCmp = Boolean.compare(a.isTest(), b.isTest());
					if (testCmp != 0) {
						return testCmp;
					}
					final var orderCmp = Integer.compare(getFolderOrder(na), getFolderOrder(nb));
					if (orderCmp != 0) {
						return orderCmp;
					}
				}
				return nameCmp;
			}
			return ta - tb;
		});
	}

	private static IClasspathDescriptor getMutableClasspath(IJavaProject project) throws CoreException {
		final var classpath = new ClasspathDescriptor(true);
		// Mark all the existing classpath entries in order to be not ignored when the getEntries() function is invoked.
		// It is preserving all the entities in the replied classspath.
		final var pfp = project.getProject().getFullPath();
		for (final var cpe : project.getRawClasspath()) {
			if (!pfp.equals(cpe.getPath())) {
				classpath.addEntry(cpe);
			}
		}
		return classpath;
	}

	private void configureSarlProject(IMavenProjectFacade facade, SARLConfiguration config,
			IClasspathDescriptor classpath, boolean addTestFolders, IProgressMonitor monitor) throws CoreException {
		final var subm = SubMonitor.convert(monitor, 2);
		addSourceFolders(facade, config, classpath, addTestFolders, subm.newChild(1));
		subm.worked(1);
		addPreferences(facade, config, addTestFolders, subm.newChild(1));
		subm.worked(1);
	}

	@Override
	public void unconfigure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		final var javaProject = JavaCore.create(request.mavenProjectFacade().getProject());
		if (javaProject != null) {
			final var classpath = getMutableClasspath(javaProject);
			addSarlLibraries(classpath);
			javaProject.setRawClasspath(classpath.getEntries(), null);
			MavenJdtPlugin.getDefault().getBuildpathManager().updateClasspath(request.mavenProjectFacade().getProject(), null);
		}
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
		final var subm = SubMonitor.convert(monitor, 4);
		final var facade = request.mavenProjectFacade();
		final var config = readConfiguration(request, subm.newChild(1));
		subm.worked(1);
		removeSarlLibraries(classpath);
		subm.worked(2);
		configureSarlProject(facade, config, classpath, true, subm);
	}

	@Override
	public AbstractBuildParticipant getBuildParticipant(
			IMavenProjectFacade projectFacade, MojoExecution execution,
			IPluginExecutionMetadata executionMetadata) {
		return new BuildParticipant(isEclipsePluginPackaging(projectFacade));
	}

}
