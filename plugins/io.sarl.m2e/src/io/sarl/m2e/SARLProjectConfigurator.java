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
package io.sarl.m2e;

import io.sarl.eclipse.SARLConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.buildpath.SARLClasspathContainerInitializer;
import io.sarl.lang.ui.preferences.SARLProjectPreferences;

import java.io.File;
import java.text.MessageFormat;
import java.util.BitSet;
import java.util.List;
import java.util.Properties;

import org.apache.maven.plugin.MojoExecution;
import org.apache.maven.project.MavenProject;
import org.eclipse.core.resources.IFolder;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Platform;
import org.eclipse.jdt.core.IClasspathAttribute;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaCore;
import org.eclipse.m2e.core.project.IMavenProjectFacade;
import org.eclipse.m2e.core.project.MavenProjectUtils;
import org.eclipse.m2e.core.project.configurator.AbstractProjectConfigurator;
import org.eclipse.m2e.core.project.configurator.ProjectConfigurationRequest;
import org.eclipse.m2e.jdt.IClasspathDescriptor;
import org.eclipse.m2e.jdt.IClasspathEntryDescriptor;
import org.eclipse.m2e.jdt.IJavaProjectConfigurator;
import org.eclipse.m2e.jdt.internal.ClasspathDescriptor;
import org.osgi.framework.Bundle;
import org.osgi.framework.Version;

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
	private static final String GROUPID_ATTR_NAME = "maven.groupId"; //$NON-NLS-1$
	private static final String ARTIFACTID_ATTR_NAME = "maven.artifactId"; //$NON-NLS-1$
	private static final String VERSION_ATTR_NAME = "maven.version"; //$NON-NLS-1$

	/** Invoked to add the preferences dedicated to SARL, JRE, etc.
	 *
	 * @param facade - the Maven face.
	 * @param config - the configuration.
	 * @param monitor - the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings("static-method")
	protected void addPreferences(
			IMavenProjectFacade facade, SARLConfiguration config,
			IProgressMonitor monitor) throws CoreException {
		IPath outputPath = makeProjectRelativePath(facade, config.getOutput());
		// Set the SARL preferences
		SARLProjectPreferences.setSpecificSARLConfigurationFor(
				facade.getProject(), outputPath);
	}

	/** Invoked to add the natures.
	 *
	 * @param facade - the Maven facade.
	 * @param config - the configuration.
	 * @param monitor - the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	@SuppressWarnings("static-method")
	protected void addNatures(IMavenProjectFacade facade,
			SARLConfiguration config, IProgressMonitor monitor) throws CoreException {
		addNature(facade.getProject(), SARLConfig.XTEXT_NATURE_ID, monitor);
		addNature(facade.getProject(), SARLConfig.NATURE_ID, monitor);
	}

	private static IPath makeFullPath(IMavenProjectFacade facade, File file) {
		assert (file != null);
		IProject project = facade.getProject();
		IPath path;
		if (!file.isAbsolute()) {
			path =  Path.fromOSString(file.getPath());
		} else {
			path = MavenProjectUtils.getProjectRelativePath(project, file.getAbsolutePath());
		}
		return project.getFullPath().append(path);
	}

	private static IPath makeProjectRelativePath(IMavenProjectFacade facade, File file) {
		assert (file != null);
		IProject project = facade.getProject();
		if (!file.isAbsolute()) {
			return Path.fromOSString(file.getPath());
		}
		return MavenProjectUtils.getProjectRelativePath(project, file.getAbsolutePath());
	}

	private static IFolder makeFolder(IMavenProjectFacade facade, IPath path) {
		return facade.getProject().getFolder(path.makeRelativeTo(facade.getProject().getFullPath()));
	}

	/** Invoked to add the source folders.
	 *
	 * @param facade - the facade of the Maven project.
	 * @param config - the configuration.
	 * @param classpath - the project classpath.
	 * @param monitor - the monitor.
	 * @throws CoreException if cannot add the source folders.
	 */
	protected void addSourceFolders(
			IMavenProjectFacade facade, SARLConfiguration config,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {

		assertHasNature(facade.getProject(), JavaCore.NATURE_ID);
		assertHasNature(facade.getProject(), SARLConfig.XTEXT_NATURE_ID);
		assertHasNature(facade.getProject(), SARLConfig.NATURE_ID);

		String encoding = config.getEncoding();

		IClasspathEntryDescriptor descriptor;

		// Add the source folders
		IPath inputPath = makeFullPath(facade, config.getInput());
		classpath.addSourceEntry(
				inputPath,
				facade.getOutputLocation(),
				true);
		IFolder inputFolder = makeFolder(facade, inputPath);
		if (encoding != null && inputFolder != null && inputFolder.exists()) {
			inputFolder.setDefaultCharset(encoding, monitor);
		}

		IPath outputPath = makeFullPath(facade, config.getOutput());
		descriptor = classpath.addSourceEntry(
				outputPath,
				facade.getOutputLocation(),
				true);
		descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
		IFolder outputFolder = makeFolder(facade, outputPath);
		if (encoding != null && outputFolder != null && outputFolder.exists()) {
			outputFolder.setDefaultCharset(encoding, monitor);
		}

		// Add the test folders
		IPath testInputPath = makeFullPath(facade, config.getTestInput());
		classpath.addSourceEntry(
				testInputPath,
				facade.getOutputLocation(),
				true);
		IFolder testInputFolder = makeFolder(facade, testInputPath);
		if (encoding != null && testInputFolder != null && testInputFolder.exists()) {
			testInputFolder.setDefaultCharset(encoding, monitor);
		}

		IPath testOutputPath = makeFullPath(facade, config.getTestOutput());
		descriptor = classpath.addSourceEntry(
				testOutputPath,
				facade.getOutputLocation(),
				true);
		descriptor.setClasspathAttribute(IClasspathAttribute.IGNORE_OPTIONAL_PROBLEMS, Boolean.TRUE.toString());
		IFolder testOutputFolder = makeFolder(facade, testOutputPath);
		if (encoding != null && testOutputFolder != null && testOutputFolder.exists()) {
			testOutputFolder.setDefaultCharset(encoding, monitor);
		}
	}

	/** Replies the configuration value.
	 *
	 * @param <T> - the expected type.
	 * @param project - the project.
	 * @param parameter - the parameter name.
	 * @param asType - the expected type.
	 * @param mojoExecution - the mojo execution.
	 * @param monitor - the monitor.
	 * @param defaultValue - the default value.
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
	 * @param request - the configuration request.
	 * @param monitor - the monitor.
	 * @return the SARL configuration.
	 * @throws CoreException if something wrong appends.
	 */
	protected SARLConfiguration readConfiguration(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {

		SARLConfiguration initConfig = null;
		SARLConfiguration compileConfig = null;

		List<MojoExecution> mojos = getMojoExecutions(request, monitor);
		for (MojoExecution mojo : mojos) {
			String goal = mojo.getGoal();
			switch(goal) {
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
	 * @param request - the request.
	 * @param mojo - the mojo execution.
	 * @param monitor - the monitor.
	 * @return the configuration.
	 * @throws CoreException
	 */
	private SARLConfiguration readInitializeConfiguration(
			ProjectConfigurationRequest request, MojoExecution mojo, IProgressMonitor monitor)
					throws CoreException {
		SARLConfiguration config = new SARLConfiguration();

		MavenProject project = request.getMavenProject();

		File input = getParameterValue(project, "input", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_SARL));
		File output = getParameterValue(project, "output", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_SOURCE_GENERATED));
		File testInput = getParameterValue(project, "testInput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_SARL));
		File testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor, //$NON-NLS-1$
				new File(SARLConfig.FOLDER_TEST_SOURCE_GENERATED));

		config.setInput(input);
		config.setOutput(output);
		config.setTestInput(testInput);
		config.setTestOutput(testOutput);

		return config;
	}

	/** Read the configuration for the Compilation mojo.
	 *
	 * @param request - the request.
	 * @param mojo - the mojo execution.
	 * @param monitor - the monitor.
	 * @return the configuration.
	 * @throws CoreException
	 */
	private SARLConfiguration readCompileConfiguration(
			ProjectConfigurationRequest request, MojoExecution mojo, IProgressMonitor monitor)
					throws CoreException {
		SARLConfiguration config = new SARLConfiguration();

		MavenProject project = request.getMavenProject();

		File input = getParameterValue(project, "input", File.class, mojo, monitor); //$NON-NLS-1$
		File output = getParameterValue(project, "output", File.class, mojo, monitor); //$NON-NLS-1$
		File testInput = getParameterValue(project, "testInput", File.class, mojo, monitor); //$NON-NLS-1$
		File testOutput = getParameterValue(project, "testOutput", File.class, mojo, monitor); //$NON-NLS-1$

		config.setInput(input);
		config.setOutput(output);
		config.setTestInput(testInput);
		config.setTestOutput(testOutput);

		String inputCompliance = getParameterValue(project, "source", String.class, mojo, monitor); //$NON-NLS-1$
		String outputCompliance = getParameterValue(project, "target", String.class, mojo, monitor); //$NON-NLS-1$

		config.setInputCompliance(inputCompliance);
		config.setOutputCompliance(outputCompliance);

		String encoding = getParameterValue(project, "encoding", String.class, mojo, monitor); //$NON-NLS-1$
		config.setEncoding(encoding);

		return config;
	}

	/** Remove any reference to the SARL libraries from the given classpath.
	 *
	 * @param classpath - the classpath to update.
	 */
	@SuppressWarnings("static-method")
	protected void removeSarlLibraries(IClasspathDescriptor classpath) {
		classpath.removeEntry(SARLClasspathContainerInitializer.CONTAINER_ID);
	}

	/** Add the SARL libraries into the given classpath.
	 *
	 * @param classpath - the classpath to update.
	 */
	@SuppressWarnings("static-method")
	protected void addSarlLibraries(IClasspathDescriptor classpath) {
		IClasspathEntry entry = JavaCore.newContainerEntry(SARLClasspathContainerInitializer.CONTAINER_ID);
		classpath.addEntry(entry);
	}

	private static int compareVersions(String v1, String v2) {
		Version a = Version.parseVersion(v1);
		Version b = Version.parseVersion(v2);
		return a.compareTo(b);
	}

	private static void setVersion(Properties props, String propName, String value, String minValue) {
		String currentVersion = props.getProperty(propName);
		String newVersion = value;

		if (compareVersions(currentVersion, newVersion) > 0) {
			newVersion = currentVersion;
		}

		if (compareVersions(newVersion, minValue) < 0) {
			props.setProperty(propName, minValue);
		} else {
			props.setProperty(propName, newVersion);
		}
	}

	private static void forceMavenCompilerConfiguration(IMavenProjectFacade facade, SARLConfiguration config) {
		Properties props = facade.getMavenProject().getProperties();
		setVersion(props, "maven.compiler.source", config.getInputCompliance(), //$NON-NLS-1$
				SARLConfig.MINIMAL_JRE_VERSION);
		setVersion(props, "maven.compiler.target", config.getOutputCompliance(), //$NON-NLS-1$
				SARLConfig.MINIMAL_JRE_VERSION);
		String encoding = config.getEncoding();
		if (encoding != null && !encoding.isEmpty()) {
			props.setProperty("maven.compiler.encoding", encoding); //$NON-NLS-1$
		}
	}

	@Override
	public void configure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		SARLConfiguration config = readConfiguration(request, monitor);
		forceMavenCompilerConfiguration(request.getMavenProjectFacade(), config);
		addNatures(request.getMavenProjectFacade(), config, monitor);
	}

	@Override
	public void unconfigure(ProjectConfigurationRequest request,
			IProgressMonitor monitor) throws CoreException {
		IJavaProject javaProject = JavaCore.create(request.getProject());
		IClasspathDescriptor classpath = new ClasspathDescriptor(javaProject);
		addSarlLibraries(classpath);
	}

	private static void validateSARLLibraryVersion(String mavenVersion) throws CoreException {
		Bundle bundle = Platform.getBundle(SARL_LANG_BUNDLE_NAME);
		if (bundle == null) {
			throw new CoreException(SARLMavenEclipsePlugin.createStatus(IStatus.ERROR,
					MessageFormat.format(Messages.SARLProjectConfigurator_0, SARL_LANG_BUNDLE_NAME)));
		}
		Version bundleVersion = bundle.getVersion();
		if (bundleVersion == null) {
			throw new CoreException(SARLMavenEclipsePlugin.createStatus(IStatus.ERROR,
					MessageFormat.format(Messages.SARLProjectConfigurator_1, SARL_LANG_BUNDLE_NAME)));
		}
		Version minVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor(), 0);
		Version maxVersion = new Version(bundleVersion.getMajor(), bundleVersion.getMinor() + 1, 0);

		assert (minVersion != null && maxVersion != null);
		Version mvnVersion = SARLMavenEclipsePlugin.parseMavenVersion(mavenVersion);
		int compare = SARLEclipsePlugin.compareVersionToRange(mvnVersion, minVersion, maxVersion);
		if (compare < 0) {
			throw new CoreException(SARLMavenEclipsePlugin.createStatus(IStatus.ERROR,
					MessageFormat.format(
							Messages.SARLProjectConfigurator_2,
							SARL_GROUP_ID, SARL_ARTIFACT_ID, mavenVersion, minVersion.toString())));
		} else if (compare > 0) {
			throw new CoreException(SARLMavenEclipsePlugin.createStatus(IStatus.ERROR,
					MessageFormat.format(
							Messages.SARLProjectConfigurator_3,
							SARL_GROUP_ID, SARL_ARTIFACT_ID, mavenVersion, maxVersion.toString())));
		}
	}

	private static boolean validateSARLLibraryVersion(IClasspathDescriptor classpath) throws CoreException {
		for (IClasspathEntry dep : classpath.getEntries()) {
			IClasspathAttribute[] attrs = dep.getExtraAttributes();
			BitSet flags = new BitSet(3);
			String version = null;
			for (int i = 0; version == null && flags.cardinality() != 3 && i < attrs.length; ++i) {
				IClasspathAttribute attr = attrs[i];
				if (GROUPID_ATTR_NAME.equals(attr.getName())
						&& SARL_GROUP_ID.equals(attr.getValue())) {
					flags.set(0);
				} else if (ARTIFACTID_ATTR_NAME.equals(attr.getName())
						&& SARL_ARTIFACT_ID.equals(attr.getValue())) {
					flags.set(1);
				} else if (VERSION_ATTR_NAME.equals(attr.getName())) {
					flags.set(2);
					version = attr.getValue();
				}
			}
			if (flags.cardinality() == 3 && version != null) {
				validateSARLLibraryVersion(version);
				return true;
			}
		}

		return false;
	}

	@Override
	public void configureClasspath(IMavenProjectFacade facade,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {
		validateSARLLibraryVersion(classpath);
	}

	@Override
	public void configureRawClasspath(ProjectConfigurationRequest request,
			IClasspathDescriptor classpath, IProgressMonitor monitor)
					throws CoreException {
		IMavenProjectFacade facade = request.getMavenProjectFacade();
		SARLConfiguration config = readConfiguration(request, monitor);
		removeSarlLibraries(classpath);
		addSourceFolders(facade, config, classpath, monitor);
		addPreferences(facade, config, monitor);
	}

}
