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
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URL;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.Set;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Injector;
import com.google.inject.Module;
import com.google.inject.Provider;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.compiler.batch.CleaningPolicy;
import io.sarl.lang.compiler.batch.OptimizationLevel;
import io.sarl.lang.compiler.batch.SarlBatchCompiler;
import io.sarl.lang.maven.compiler.compiler.JavaCompiler;
import io.sarl.lang.maven.compiler.utils.MavenJulHandler;
import io.sarl.lang.maven.compiler.utils.MavenProjectResourceSetProvider;

/** Abstract mojo that is able to use the SARL batch compiler.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
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

	@Parameter(readonly = true, defaultValue = "${basedir}/.settings/io.sarl.lang.SARL.prefs")
	private String propertiesFileLocation;

	/** Indicates if all the SARL warnings are considered as errors.
	 * @since 0.13
	 */
	@Parameter(defaultValue = "false")
	private boolean warningsAsErrors;
	
	private List<File> bufferedClassPath;

	private List<File> bufferedTestClassPath;

	private List<File> bufferedModulePath;

	private List<File> bufferedTestModulePath;

	/** Overriding of the issue severities.
	 *
	 * @since 0.15
	 */
	@Parameter
    private Map<String, String> issueSeverities;

    private Map<String, Severity> issueSeverityOverrides;

    /** Convert the string representation of an issue severity to its enumeration instance.
     *
     * @param severity the string representation of the severity.
     * @return the severity instance, or {@code null} if the given string representating cannot be converted to a severity instance.
     * @since 0.15
     */
    @SuppressWarnings("static-method")
	protected Severity toSeverity(String severity) {
    	if (!Strings.isEmpty(severity)) {
    		final var item = severity.toUpperCase();
    		for (final var candidate : Severity.values()) {
    			if (item.equals(candidate.name())) {
    				return candidate;
    			}
    		}
    	}
    	return null;
    }
    
	/** Replies the overrides of the issue severities that are provided in the POM file.
	 *
	 * @since 0.15
	 * @throws MojoFailureException if a severity string is not correctly formatted.
	 */
	protected Map<String, Severity> getIssueSeverityOverrides() throws MojoFailureException {
		if (this.issueSeverityOverrides == null) {
			this.issueSeverityOverrides = new HashMap<>();
			if (this.issueSeverities != null && !this.issueSeverities.isEmpty()) {
				for (final var entry : this.issueSeverities.entrySet()) {
					if (!Strings.isEmpty(entry.getKey())) {
						final var severityStr = entry.getValue();
						if (Strings.isEmpty(severityStr)) {
							throw new MojoFailureException(
									MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_14, entry.getKey()));
						}
						final var severity = toSeverity(severityStr);
						if (severity == null) {
							throw new MojoFailureException(
									MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_15, entry.getKey(), severityStr));
						}
						this.issueSeverityOverrides.put(entry.getKey(), severity);
					}
				}
			}
		}
		return Collections.unmodifiableMap(this.issueSeverityOverrides);
	}

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
	 * included jar files contains API for newer JRE, that causes incompatible
	 * class format.
	 *
	 * @param currentClassPath is the classpath to fix.
	 * @param classpathName is the name of the classpath to fix.
	 * @return the fixed classpath.
	 * @since 0.12
	 */
	protected List<File> fixJreClassPathFiles(List<File> currentClassPath, String classpathName) {
		final var conflictPattern = Pattern.compile(CONFLICTING_JAR_PATTERN);
		final var numPattern = Pattern.compile(NUM_PATTERN);
		final var newClassPath = new ArrayList<File>(currentClassPath.size());
		final var currentVersion = parseInt(numPattern, System.getProperty("java.version")); //$NON-NLS-1$
		for (final var file : currentClassPath) {
			final var basename = file.getName();
			final var matcher = conflictPattern.matcher(basename);
			if (matcher.find()) {
				final var version = parseInt(numPattern, matcher.group(1));
				if (version <= currentVersion) {
					newClassPath.add(file);
				} else {
					getLogger().info(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_11, basename, classpathName));
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
		final var conflictPattern = Pattern.compile(CONFLICTING_JAR_PATTERN);
		final var numPattern = Pattern.compile(NUM_PATTERN);
		final var newClassPath = new ArrayList<URL>(currentClassPath.size());
		final var currentVersion = parseInt(numPattern, System.getProperty("java.version")); //$NON-NLS-1$
		for (final var file : currentClassPath) {
			final var basename = FileSystem.largeBasename(file);
			final var matcher = conflictPattern.matcher(basename);
			if (matcher.find()) {
				final var version = parseInt(numPattern, matcher.group(1));
				if (version <= currentVersion) {
					newClassPath.add(file);
				} else {
					getLogger().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_11, basename, classpathName));
				}
			} else {
				newClassPath.add(file);
			}
		}
		return newClassPath;
	}

	private static int parseInt(Pattern pattern, String text) {
		final var matcher = pattern.matcher(text);
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
	@SuppressWarnings({"unchecked"})
	protected synchronized void prepareExecution() throws MojoExecutionException {
		if (this.injector == null) {
			try {
				var classLoader = getClass().getClassLoader();
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
				final var setup = classLoader.loadClass(SARLStandaloneSetup.class.getName());
				final var method = setup.getDeclaredMethod("doSetup"); //$NON-NLS-1$
				final var mainInjector = (Injector) method.invoke(null);
				// Create the plugin's injection module
				final var innerModuleType = (Class<? extends Module>) classLoader.loadClass(MavenPrivateModule.class.getName());
				final var innerModule = innerModuleType.getConstructor(AbstractSarlBatchCompilerMojo.class).newInstance(this);
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
	 * @return {@code true} for generating the inline annotations.
	 */
	protected abstract boolean getGenerateInlines();

	/** Replies if the pure annotations must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the pure annotations.
	 */
	protected abstract boolean getGeneratePures();

	/** Replies if the trace files must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the trace files.
	 */
	protected abstract boolean getGenerateTraceFiles();

	/** Replies if the storage files must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the storage files.
	 */
	protected abstract boolean getGenerateStorageFiles();

	/** Replies if the equality test functions must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateEqualityTestFunctions();

	/** Replies if the toString functions must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateToStringFunctions();

	/** Replies if the clone functions must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the functions.
	 * @since 0.8
	 */
	protected abstract boolean getGenerateCloneFunctions();

	/** Replies if the serial number fields must be generated by the SARL compiler.
	 *
	 * @return {@code true} for generating the fields.
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
			var mavenTestSkip = false;
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
			var mavenCompileSkip = false;
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
		var sarlCompileSkip = false;
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
	protected void compile(List<File> classPath, List<File> modulePath, List<File> sourcePaths, File sarlOutputPath,
			File classOutputPath) throws MojoExecutionException, MojoFailureException {
		final var compiler = getBatchCompiler();
		final var project = getProject();
		compiler.setResourceSetProvider(new MavenProjectResourceSetProvider(project));
		final var filtered = Iterables.filter(sourcePaths, input -> input.isDirectory());
		if (Iterables.isEmpty(filtered)) {
			final var dir = Iterables.toString(sourcePaths);
			getLogger().info(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_1, dir));
			return;
		}
		final var baseDir = project.getBasedir().getAbsolutePath();
		if (getLogger().isDebugEnabled()) {
			final var out = new StringBuilder();
			out.append("sarlOutputPath = "); //$NON-NLS-1$
			out.append(sarlOutputPath);
			out.append("\nclassOutputPath = "); //$NON-NLS-1$
			out.append(classOutputPath);
			getLogger().debug(out.toString());
		}
		compiler.setSarlCompilationEnable(!isSarlJvmInferrerSkipped());
		final var compilerType = getJavaCompiler();

		final var logger = Logger.getLogger(getClass().getName());
		logger.setUseParentHandlers(false);
		for (final var h : logger.getHandlers()) {
			logger.removeHandler(h);
		}
		logger.addHandler(new MavenJulHandler(getLogger()));
		compiler.setLogger(logger);
		
		compiler.setJavaPostCompilationEnable(compilerType != JavaCompiler.NONE);
		for (final var severityOverride : getIssueSeverityOverrides().entrySet()) {
			compiler.setWarningSeverity(severityOverride.getKey(), severityOverride.getValue());
		}
		compiler.setReportWarningsAsErrors(this.warningsAsErrors);
		compiler.setOptimizationLevel(getOptimization());
		compiler.setClassOutputPath(classOutputPath);
		compiler.setJavaSourceVersion(getSourceVersion());
		compiler.setBasePath(baseDir);
		compiler.setTempDirectory(getTempDirectory());
		compiler.setCleaningPolicy(CleaningPolicy.NO_CLEANING);
		compiler.setClassPath(classPath);
		compiler.setModulePath(modulePath);
		final var filteredSourcePaths = Lists.<File>newArrayList(filtered);
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

		final var builder = new StringBuilder();
		for (final var identifier : getExtraGenerators()) {
			if (builder.length() > 0) {
				builder.append(File.pathSeparator);
			}
			builder.append(identifier);
		}
		compiler.setExtraLanguageGenerators(builder.toString());

		compiler.setIssueMessageFormatter((severity, issue, uriToProblem) -> {
			final String filename;
			if (uriToProblem != null) {
				filename = uriToProblem.toFileString();
			} else {
				filename = Messages.AbstractSarlBatchCompilerMojo_2;
			}
			return MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_3,
					filename, issue.getLineNumber(),
					issue.getColumn(), issue.getMessage(), issue.getCode());
		});
		final var errorMessage = new String[] {null};
		compiler.addIssueMessageListener((severity, issue, uri, message) -> {
			var isError = severity == Severity.ERROR || issue.isSyntaxError();
			if (isError && Strings.isEmpty(errorMessage[0])) {
				errorMessage[0] = message;
			}
		});
		if (!compiler.compile()) {
			final var dir = new StringBuilder();
			for (final var file : filtered) {
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
			final var file = new File(this.propertiesFileLocation);
			if (file.canRead()) {
				final var sarlSettings = new Properties();
				try (var stream = new FileInputStream(file)) {
					sarlSettings.load(stream);
					final var sarlOutputDirProp = sarlSettings.getProperty("outlet.DEFAULT_OUTPUT.directory", null); //$NON-NLS-1$
					if (sarlOutputDirProp != null) {
						final var srcDir = new File(sourceDirectory);
						getLogger().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_7,
								srcDir.getPath(), Boolean.valueOf(srcDir.exists())));
						if (srcDir.exists() && srcDir.getParent() != null) {
							final var path = new File(srcDir.getParent(), sarlOutputDirProp).getPath();
							getLogger().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_8, sarlOutputDirProp));
							return path;
						}
					}
				} catch (FileNotFoundException e) {
					getLogger().warn(e.getLocalizedMessage(), e);
				} catch (IOException e) {
					getLogger().warn(e.getLocalizedMessage(), e);
				}
			} else {
				getLogger().debug(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_9, this.propertiesFileLocation));
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
		for (final var file : files) {
			if (duplicateAvoider.add(file)) {
				final var fileObj = new File(file);
				if (fileObj.exists()) {
					classpathFiles.add(fileObj);
				} else if (isMandatory) {
					if (!fileObj.mkdirs()) {
						getLogger().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_10, file));
					} else {
						classpathFiles.add(fileObj);
					}
				} else if (warnIfMissed) {
					getLogger().warn(MessageFormat.format(Messages.AbstractSarlBatchCompilerMojo_10, file));
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
		final var duplicateAvoider = new LinkedHashSet<String>();
		final var classpathFiles = new ArrayList<File>();
		final var project = getProject();
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
		for (final var dep : project.getArtifacts()) {
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
		final var duplicateAvoider = new LinkedHashSet<String>();
		final var classpathFiles = new ArrayList<File>();
		final var project = getProject();
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
		for (final var dep : project.getArtifacts()) {
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
	@SuppressWarnings("static-method")
	protected List<File> buildModulePath() throws MojoExecutionException {
		final var modulePathFiles = new ArrayList<File>();
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
	@SuppressWarnings("static-method")
	protected List<File> buildTestModulePath() throws MojoExecutionException {
		final var modulePathFiles = new ArrayList<File>();
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
