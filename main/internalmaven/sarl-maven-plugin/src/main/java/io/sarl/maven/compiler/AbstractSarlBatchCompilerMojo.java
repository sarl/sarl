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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.lang.reflect.Method;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Handler;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.inject.Provider;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Injector;
import com.google.inject.Module;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.toolchain.ToolchainManager;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.compiler.batch.CleaningPolicy;
import io.sarl.lang.compiler.batch.OptimizationLevel;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;

/** Abstract mojo that is able to use the SARL batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class AbstractSarlBatchCompilerMojo extends AbstractSarlMojo {

	/** Regular expression that is used for detecting the potential conflicts with
	 * the Java libraries.
	 *
	 * @since 0.12
	 */
	private static final String CONFLICTING_JAR_PATTERN = "^java([0-9]+)api\\.jar$"; //$NON-NLS-1$

	/** Regular expression for integer numbers.
	 *
	 * @since 0.12
	 */
	private static final String NUM_PATTERN = "^([0-9]+)"; //$NON-NLS-1$

	private Injector injector;

	private Provider<SarlBatchCompiler> sarlBatchCompilerProvider;

	private ReflectExtensions reflect;

	@Component
	private ToolchainManager toolchainManager;

	@Parameter(readonly = true, defaultValue = "${basedir}/.settings/io.sarl.lang.SARL.prefs")
	private String propertiesFileLocation;

	private List<File> bufferedClassPath;

	private List<File> bufferedTestClassPath;

	private List<File> bufferedModulePath;

	private List<File> bufferedTestModulePath;

	/** Replies if the classpath must be fixed.
	 *
	 * @return {@code true} if the classpath is fixed.
	 * @since 0.13
	 */
	protected abstract boolean isFixingJavaClasspathForJre();

	/** Replies if the plugin is run into with Tycho Eclipse RCP environment.
	 *
	 * @return {@code true} if the tycho is activated.
	 * @since 0.13
	 */
	protected abstract boolean isTychoEnvironment();

	/** Fix the classpath because the JDT libraries that are included by tycho
	 * automatically add "javaXapi.jar" on the classpath. Sometimes, the
	 * included jar files contains API for newer JSE, that causes incompatible
	 * class format.
	 *
	 * @param currentClassPath is the classpath to fix.
	 * @param classpathName is the name of the classpath to fix.
	 * @return the fixed classpath.
	 * @since 0.12
	 */
	protected List<File> fixJreClassPathFiles(List<File> currentClassPath, String classpathName) {
		final Pattern conflictPattern = Pattern.compile(CONFLICTING_JAR_PATTERN);
		final Pattern numPattern = Pattern.compile(NUM_PATTERN);
		final List<File> newClassPath = new ArrayList<>(currentClassPath.size());
		final int currentVersion = parseInt(numPattern, System.getProperty("java.version"));
		for (final File file : currentClassPath) {
			final String basename = file.getName();
			final Matcher matcher = conflictPattern.matcher(basename);
			if (matcher.find()) {
				final int version = parseInt(numPattern, matcher.group(1));
				if (version <= currentVersion) {
					newClassPath.add(file);
				} else {
					getLog().info(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_11, basename, classpathName));
				}
			} else {
				newClassPath.add(file);
			}
		}
		return newClassPath;
	}

	/** Fix the classpath because the JDT libraries that are included by tycho
	 * automatically which adds "javaXapi.jar" on the classpath. Sometimes, the
	 * included jar files contains API for newer JSE, that causes incompatible
	 * class format.
	 *
	 * @param currentClassPath is the classpath to fix.
	 * @param classpathName is the name of the classpath to fix.
	 * @return the fixed classpath.
	 * @since 0.13
	 */
	protected List<URL> fixJreClassPathURLs(List<URL> currentClassPath, String classpathName) {
		final Pattern conflictPattern = Pattern.compile(CONFLICTING_JAR_PATTERN);
		final Pattern numPattern = Pattern.compile(NUM_PATTERN);
		final List<URL> newClassPath = new ArrayList<>(currentClassPath.size());
		final int currentVersion = parseInt(numPattern, System.getProperty("java.version"));
		for (final URL file : currentClassPath) {
			final String basename = FileSystem.largeBasename(file);
			final Matcher matcher = conflictPattern.matcher(basename);
			if (matcher.find()) {
				final int version = parseInt(numPattern, matcher.group(1));
				if (version <= currentVersion) {
					newClassPath.add(file);
				} else {
					getLog().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_11, basename, classpathName));
				}
			} else {
				newClassPath.add(file);
			}
		}
		return newClassPath;
	}

	private static int parseInt(Pattern pattern, String text) {
		final Matcher matcher = pattern.matcher(text);
		if (matcher.find()) {
			try {
				return Integer.parseInt(matcher.group(1));
			} catch (Throwable ex) {
				//
			}
		}
		return 0;
	}

	@Override
	@SuppressWarnings({"resource", "unchecked"})
	protected synchronized void prepareExecution() throws MojoExecutionException {
		if (this.injector == null) {
			try {
				ClassLoader classLoader = getClass().getClassLoader();
				/*
				// Override the standard class loader in order to remove the conflicting
				// internal libs (in *.jar/lib/) from the classpath
				if (isTychoEnvironment() && isFixingJavaClasspathForJre()) {
					getLog().warn(Messages.AbstractSarlBatchCompilerMojo_13);
					final ClassLoader realm = getClass().getClassLoader();
					classLoader = new TychoCompliantClassLoader(realm);
					Thread.currentThread().setContextClassLoader(classLoader);
				}
				*/
				// Create the SARL injector by using a reflect approach in order to use the
				// class loader that is defined above
				//
				// Create the SARL setup and create the injector
				final Class<?> setup = classLoader.loadClass(SARLStandaloneSetup.class.getName());
				final Method method = setup.getDeclaredMethod("doSetup");
				final Injector mainInjector = (Injector) method.invoke(null);
				// Create the plugin's injection module
				final Class<? extends Module> innerModuleType = (Class<? extends Module>) classLoader.loadClass(MavenPrivateModule.class.getName());
				final Module innerModule = innerModuleType.getConstructor(AbstractSarlBatchCompilerMojo.class).newInstance(this);
				// Create the sub-injector
				this.injector = mainInjector.createChildInjector(Arrays.asList(innerModule));
				//
				/*classLoader.loadClass("javax.tools.JavaCompiler");
				URL res0 = this.injector.getClass().getClassLoader().getResource("javax/tools/JavaCompiler.class");
				URL res1 = classLoader.getResource("javax/tools/JavaCompiler.class");
				getLog().info("RES0 = " + res0);
				getLog().info("RES1 = " + res1);
				getLog().info("CLS = " + classLoader.toString());
				getLog().info("CLS2 = " + mainInjector.getClass().getClassLoader());
				getLog().info("CLS3 = " + this.injector.getClass().getClassLoader());
				this.injector.getClass().getClassLoader().loadClass("javax.tools.JavaCompiler");*/
			} catch (Throwable ex) {
				throw new RuntimeException(ex);
			}
		}
		if (this.sarlBatchCompilerProvider == null) {
			this.sarlBatchCompilerProvider = this.injector.getProvider(SarlBatchCompiler.class);
		}
		if (this.reflect == null) {
			this.reflect = this.injector.getInstance(ReflectExtensions.class);
		}
		if (this.sarlBatchCompilerProvider == null || this.reflect == null) {
			throw new MojoExecutionException(Messages.AbstractSarlBatchCompilerMojo_0);
		}
	}

	/** Replies the batch compiler for SARL.
	 *
	 * @return the batch compiler.
	 */
	protected SarlBatchCompiler getBatchCompiler() {
		return this.sarlBatchCompilerProvider.get();
	}

	/** Replies the current project.
	 *
	 * @return the current project.
	 */
	protected MavenProject getProject() {
		return this.mavenHelper.getSession().getCurrentProject();
	}

	/** Replies the version of the source.
	 *
	 * @return the source version.
	 */
	protected abstract String getSourceVersion();

	/** Replies the encoding of the source.
	 *
	 * @return the encoding.
	 */
	protected abstract String getEncoding();

	/** Replies if the inline annotations must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the inline annotations.
	 */
	protected abstract boolean getGenerateInlines();

	/** Replies if the pure annotations must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the pure annotations.
	 */
	protected abstract boolean getGeneratePures();

	/** Replies if the trace files must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the trace files.
	 */
	protected abstract boolean getGenerateTraceFiles();

	/** Replies if the storage files must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the storage files.
	 */
	protected abstract boolean getGenerateStorageFiles();

	/** Replies if the equality test functions must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateEqualityTestFunctions();

	/** Replies if the toString functions must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateToStringFunctions();

	/** Replies if the clone functions must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateCloneFunctions();

	/** Replies if the serial number fields must be generated by the SARL compiler.
	 *
	 * @return <code>true</code> for generating the fields.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateSerialNumberFields();

	/** Replies the list of the extra-language generators' identifiers that should be enabled.
	 *
	 * @return the list of extra-language generators' identifiers.
	 * @since 0.8
	 */
	protected abstract String[] getExtraGenerators();

	/** Replies the Java compiler to use.
	 *
	 * @return the Java compiler, never {@code null}.
	 * @since 0.8
	 */
	protected abstract JavaCompiler getJavaCompiler();

	/** Replies the optimization level to use.
	 *
	 * @return the optimization level, never {@code null}.
	 * @since 0.8
	 */
	protected abstract OptimizationLevel getOptimization();

	/** Replies if the mojo is used within a test code compilation context.
	 *
	 * @return {@code true} if this mojo is used within a test phase.
	 * @since 0.8
	 */
	protected abstract boolean isTestContext();

	@Override
	protected boolean isSkipped() {
		if (isTestContext()) {
			// Check the general Maven test skipping flag
			boolean mavenTestSkip = false;
			try {
				mavenTestSkip = Boolean.parseBoolean(this.session.getUserProperties().getProperty(MAVEN_TEST_SKIP_NAME, "false")); //$NON-NLS-1$
			} catch (Throwable exception) {
				mavenTestSkip = false;
			}
			if (mavenTestSkip) {
				return true;
			}
			try {
				mavenTestSkip = Boolean.parseBoolean(this.session.getUserProperties().getProperty(SARL_TEST_SKIP_NAME, "false")); //$NON-NLS-1$
			} catch (Throwable exception) {
				mavenTestSkip = false;
			}
			if (mavenTestSkip) {
				return true;
			}
		} else {
			// Check the general SARL compile skipping flag
			boolean mavenCompileSkip = false;
			try {
				mavenCompileSkip = Boolean.parseBoolean(this.session.getUserProperties().getProperty(SARL_COMPILE_SKIP_NAME, "false")); //$NON-NLS-1$
			} catch (Throwable exception) {
				mavenCompileSkip = false;
			}
			if (mavenCompileSkip) {
				return true;
			}
		}
		return super.isSkipped();
	}

	/** Replies if the SARL JVM Inferring should be skipped or not.
	 *
	 * @return {@code true} if the JVM Inferring is skipped.
	 * @since 0.12
	 */
	protected boolean isSarlJvmInferrerSkipped() {
		boolean sarlCompileSkip = false;
		try {
			sarlCompileSkip = Boolean.parseBoolean(this.session.getUserProperties().getProperty(SARL_JVMINFERRER_SKIP_NAME, "false")); //$NON-NLS-1$
		} catch (Throwable exception) {
			sarlCompileSkip = false;
		}
		return sarlCompileSkip;
	}

	/** Run compilation.
	 *
	 * @param classPath the classpath
	 * @param modulePath the module-path
	 * @param sourcePaths the source paths.
	 * @param sarlOutputPath the output path for receiving the SARL code.
	 * @param classOutputPath the output path for receiving the Java class files.
	 * @throws MojoExecutionException if error.
	 * @throws MojoFailureException if failure.
	 */
	@SuppressWarnings({ "checkstyle:npathcomplexity" })
	protected void compile(List<File> classPath, List<File> modulePath, List<File> sourcePaths, File sarlOutputPath,
			File classOutputPath) throws MojoExecutionException, MojoFailureException {
		final SarlBatchCompiler compiler = getBatchCompiler();
		final MavenProject project = getProject();
		compiler.setResourceSetProvider(new MavenProjectResourceSetProvider(project));
		final Iterable<File> filtered = Iterables.filter(sourcePaths, input -> input.isDirectory());
		if (Iterables.isEmpty(filtered)) {
			final String dir = Iterables.toString(sourcePaths);
			getLog().info(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_1, dir));
			return;
		}
		final String baseDir = project.getBasedir().getAbsolutePath();
		if (getLog().isDebugEnabled()) {
			final StringBuilder out = new StringBuilder();
			out.append("sarlOutputPath = ");
			out.append(sarlOutputPath);
			out.append("\nclassOutputPath = ");
			out.append(classOutputPath);
			getLog().debug(out.toString());
		}
		compiler.setSarlCompilationEnable(!isSarlJvmInferrerSkipped());
		final JavaCompiler compilerType = getJavaCompiler();
		compiler.setJavaPostCompilationEnable(compilerType != JavaCompiler.NONE);
		compiler.setOptimizationLevel(getOptimization());
		compiler.setClassOutputPath(classOutputPath);
		compiler.setJavaSourceVersion(getSourceVersion());
		compiler.setBasePath(baseDir);
		compiler.setTempDirectory(getTempDirectory());
		compiler.setCleaningPolicy(CleaningPolicy.NO_CLEANING);
		compiler.setClassPath(classPath);
		compiler.setModulePath(modulePath);
		final List<File> filteredSourcePaths = Lists.newArrayList(filtered);
		compiler.setSourcePath(filteredSourcePaths);
		compiler.setOutputPath(sarlOutputPath);
		compiler.setFileEncoding(getEncoding());
		compiler.setWriteTraceFiles(getGenerateTraceFiles());
		compiler.setWriteStorageFiles(getGenerateStorageFiles());
		compiler.setGenerateInlineAnnotation(getGenerateInlines());
		compiler.setGeneratePureAnnotation(getGeneratePures());
		compiler.setGenerateEqualityTestFunctions(getGenerateEqualityTestFunctions());
		compiler.setGenerateToStringFunctions(getGenerateToStringFunctions());
		compiler.setGenerateCloneFunctions(getGenerateCloneFunctions());
		compiler.setGenerateSerialNumberFields(getGenerateSerialNumberFields());

		final StringBuilder builder = new StringBuilder();
		for (final String identifier : getExtraGenerators()) {
			if (builder.length() > 0) {
				builder.append(File.pathSeparator);
			}
			builder.append(identifier);
		}
		compiler.setExtraLanguageGenerators(builder.toString());

		final Logger logger = Logger.getLogger(getClass().getName());
		logger.setUseParentHandlers(false);
		for (final Handler h : logger.getHandlers()) {
			logger.removeHandler(h);
		}
		logger.addHandler(new MavenJulHandler(getLog()));
		compiler.setLogger(logger);

		compiler.setIssueMessageFormatter((issue, uriToProblem) -> {
			final String filename;
			if (uriToProblem != null) {
				filename = uriToProblem.toFileString();
			} else {
				filename = Messages.AbstractSarlBatchCompilerMojo_2;
			}
			return MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_3,
					filename, issue.getLineNumber(),
					issue.getColumn(), issue.getMessage());
		});
		final String[] errorMessage = new String[] {null};
		compiler.addIssueMessageListener((issue, uri, message) -> {
			if ((issue.isSyntaxError() || issue.getSeverity() == Severity.ERROR) && (Strings.isEmpty(errorMessage[0]))) {
				errorMessage[0] = message;
			}
		});
		if (!compiler.compile()) {
			final StringBuilder dir = new StringBuilder();
			for (final File file : filtered) {
				if (dir.length() > 0) {
					dir.append(File.pathSeparator);
				}
				dir.append(file.getAbsolutePath());
			}
			if (Strings.isEmpty(errorMessage[0])) {
				throw new MojoFailureException(Messages.AbstractSarlBatchCompilerMojo_4);
			}
			throw new MojoFailureException(errorMessage[0]);
		}
	}

	/** Replies temporary directory.
	 *
	 * @return the temporary directory.
	 */
	protected abstract File getTempDirectory();

	/** Read the SARL Eclipse settings for the project if existing.
	 *
	 * @param sourceDirectory the source directory.
	 * @return the path from the settings.
	 */
	protected String readSarlEclipseSetting(String sourceDirectory) {
		if (this.propertiesFileLocation != null) {
			final File file = new File(this.propertiesFileLocation);
			if (file.canRead()) {
				final Properties sarlSettings = new Properties();
				try (FileInputStream stream = new FileInputStream(file)) {
					sarlSettings.load(stream);
					final String sarlOutputDirProp = sarlSettings.getProperty("outlet.DEFAULT_OUTPUT.directory", null); //$NON-NLS-1$
					if (sarlOutputDirProp != null) {
						final File srcDir = new File(sourceDirectory);
						getLog().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_7,
								srcDir.getPath(), srcDir.exists()));
						if (srcDir.exists() && srcDir.getParent() != null) {
							final String path = new File(srcDir.getParent(), sarlOutputDirProp).getPath();
							getLog().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_8, sarlOutputDirProp));
							return path;
						}
					}
				} catch (FileNotFoundException e) {
					getLog().warn(e);
				} catch (IOException e) {
					getLog().warn(e);
				}
			} else {
				getLog().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_9, this.propertiesFileLocation));
			}
		}
		return null;
	}

	/** Update the classpath file list by adding the given files and ensuring
	 * they exists when it is necessary.
	 *
	 * @param classpathFiles the list of classpath files to update.
	 * @param duplicateAvoider the set of files that are already inside the classpath.
	 * @param isMandatory indicates if the must be physically present on the disk.
	 *     If {@code true} and the given file does not exist, it is created by using
	 *     {@link File#mkdirs()}.
	 * @param warnIfMissed indicates if a warning must be generated if the files are missed
	 *     from the disk.
	 * @param files the list of files to add to the classpath.
	 * @since 0.12
	 */
	protected void updateAtomicallyClasspathFiles(List<File> classpathFiles,
			Set<String> duplicateAvoider, boolean isMandatory,
			boolean warnIfMissed, Iterable<String> files) {
		for (final String file : files) {
			if (duplicateAvoider.add(file)) {
				final File fileObj = new File(file);
				if (fileObj.exists()) {
					classpathFiles.add(fileObj);
				} else if (isMandatory) {
					if (!fileObj.mkdirs()) {
						getLog().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_10, file));
					} else {
						classpathFiles.add(fileObj);
					}
				} else if (warnIfMissed) {
					getLog().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_10, file));
				}
			}
		}
	}

	/** Build the classpath for the standard code.
	 *
	 * @return the current classpath.
	 * @throws MojoExecutionException on failure.
	 * @since 0.12
	 * @see #buildTestClassPath()
	 * @see #getClassPath()
	 */
	protected List<File> buildClassPath() throws MojoExecutionException {
		final Set<String> duplicateAvoider = new LinkedHashSet<>();
		final List<File> classpathFiles = new ArrayList<>();
		final MavenProject project = getProject();
		updateAtomicallyClasspathFiles(
				classpathFiles, duplicateAvoider,
				true, true,
				Collections.singleton(project.getBuild().getOutputDirectory()));
		updateAtomicallyClasspathFiles(
				classpathFiles, duplicateAvoider,
				false, false,
				Collections.singleton(project.getBuild().getSourceDirectory()));
		try {
			updateAtomicallyClasspathFiles(
					classpathFiles, duplicateAvoider,
					false, true,
					project.getCompileClasspathElements());
		} catch (DependencyResolutionRequiredException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
		for (final Artifact dep : project.getArtifacts()) {
			updateAtomicallyClasspathFiles(
					classpathFiles, duplicateAvoider,
					false, true,
					Collections.singleton(dep.getFile().getAbsolutePath()));
		}
		return classpathFiles;
	}

	/** Replies the classpath for the standard code.
	 * This function build and save the classpath into a buffer.
	 *
	 * @return the current classpath.
	 * @throws MojoExecutionException on failure.
	 * @see #buildClassPath()
	 * @see #getTestClassPath()
	 */
	protected final List<File> getClassPath() throws MojoExecutionException {
		if (this.bufferedClassPath == null) {
			this.bufferedClassPath = buildClassPath();
		}
		return this.bufferedClassPath;
	}

	/** Build the classpath for the test code.
	 *
	 * @return the current classpath.
	 * @throws MojoExecutionException on failure.
	 * @since 0.12
	 * @see #getTestClassPath()
	 * @see #buildClassPath()
	 */
	protected List<File> buildTestClassPath() throws MojoExecutionException {
		final Set<String> duplicateAvoider = new LinkedHashSet<>();
		final List<File> classpathFiles = new ArrayList<>();
		final MavenProject project = getProject();
		updateAtomicallyClasspathFiles(
				classpathFiles, duplicateAvoider,
				true, true,
				Collections.singleton(project.getBuild().getTestOutputDirectory()));
		updateAtomicallyClasspathFiles(
				classpathFiles, duplicateAvoider,
				false, false,
				Collections.singleton(project.getBuild().getTestSourceDirectory()));
		try {
			updateAtomicallyClasspathFiles(
					classpathFiles, duplicateAvoider,
					false, true,
					project.getTestClasspathElements());
		} catch (DependencyResolutionRequiredException e) {
			throw new MojoExecutionException(e.getLocalizedMessage(), e);
		}
		for (final Artifact dep : project.getArtifacts()) {
			updateAtomicallyClasspathFiles(
					classpathFiles, duplicateAvoider,
					false, true,
					Collections.singleton(dep.getFile().getAbsolutePath()));
		}
		return classpathFiles;
	}

	/** Replies the classpath for the test code.
	 * This function build and save the classpath into a buffer.
	 *
	 * @return the current classpath.
	 * @throws MojoExecutionException on failure.
	 * @since 0.8
	 * @see #buildTestClassPath()
	 * @see #getClassPath()
	 */
	protected final List<File> getTestClassPath() throws MojoExecutionException {
		if (this.bufferedTestClassPath == null) {
			this.bufferedTestClassPath = buildTestClassPath();
		}
		return this.bufferedTestClassPath;
	}

	/** Build the module-path for the standard code.
	 *
	 * @return the current module-path.
	 * @throws MojoExecutionException on failure.
	 * @since 0.12
	 * @see #buildTestModulePath()
	 * @see #getModulePath()
	 */
	protected List<File> buildModulePath() throws MojoExecutionException {
		final List<File> modulePathFiles = new ArrayList<>();
		return modulePathFiles;
	}

	/** Replies the module-path for the standard code.
	 * This function build and save the module-path into a buffer.
	 *
	 * @return the current module-path.
	 * @throws MojoExecutionException on failure.
	 * @see #buildModulePath()
	 * @see #getTestModulePath()
	 */
	protected final List<File> getModulePath() throws MojoExecutionException {
		if (this.bufferedModulePath == null) {
			this.bufferedModulePath = buildModulePath();
		}
		return this.bufferedModulePath;
	}

	/** Build the module-path for the test code.
	 *
	 * @return the current module-path.
	 * @throws MojoExecutionException on failure.
	 * @since 0.12
	 * @see #getTestModulePath()
	 * @see #buildModulePath()
	 */
	protected List<File> buildTestModulePath() throws MojoExecutionException {
		final List<File> modulePathFiles = new ArrayList<>();
		return modulePathFiles;
	}

	/** Replies the module-path for the test code.
	 * This function build and save the module-path into a buffer.
	 *
	 * @return the current module-path.
	 * @throws MojoExecutionException on failure.
	 * @since 0.12
	 * @see #buildTestModulePath()
	 * @see #getModulePath()
	 */
	protected final List<File> getTestModulePath() throws MojoExecutionException {
		if (this.bufferedTestModulePath == null) {
			this.bufferedTestModulePath = buildTestModulePath();
		}
		return this.bufferedTestModulePath;
	}

}
