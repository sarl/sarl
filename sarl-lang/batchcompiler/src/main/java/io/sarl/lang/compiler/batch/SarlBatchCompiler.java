/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.compiler.batch;

import java.io.Closeable;
import java.io.File;
import java.io.FileFilter;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;
import java.util.TreeSet;
import java.util.function.Predicate;
import java.util.logging.Level;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import com.google.common.base.CharMatcher;
import com.google.common.base.Joiner;
import com.google.common.collect.Iterables;
import com.google.inject.Inject;
import com.google.inject.Provider;
import com.google.inject.name.Named;
import org.apache.log4j.Appender;
import org.apache.log4j.Category;
import org.apache.log4j.LogManager;
import org.apache.log4j.spi.HierarchyEventListener;
import org.apache.log4j.spi.LoggerFactory;
import org.apache.log4j.spi.LoggerRepository;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.emf.ecore.util.EcoreUtil;
import org.eclipse.xtend.core.macro.ProcessorInstanceForJvmTypeProvider;
import org.eclipse.xtext.Constants;
import org.eclipse.xtext.EcoreUtil2;
import org.eclipse.xtext.common.types.access.impl.ClasspathTypeProvider;
import org.eclipse.xtext.common.types.access.impl.IndexedJvmTypeAccess;
import org.eclipse.xtext.common.types.descriptions.IStubGenerator;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.generator.GeneratorContext;
import org.eclipse.xtext.generator.GeneratorDelegate;
import org.eclipse.xtext.generator.IFileSystemAccess;
import org.eclipse.xtext.generator.IOutputConfigurationProvider;
import org.eclipse.xtext.generator.JavaIoFileSystemAccess;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.generator.OutputConfigurationAdapter;
import org.eclipse.xtext.mwe.NameBasedFilter;
import org.eclipse.xtext.mwe.PathTraverser;
import org.eclipse.xtext.parser.IEncodingProvider;
import org.eclipse.xtext.resource.CompilerPhases;
import org.eclipse.xtext.resource.FileExtensionProvider;
import org.eclipse.xtext.resource.IResourceDescription;
import org.eclipse.xtext.resource.IResourceServiceProvider;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.resource.persistence.StorageAwareResource;
import org.eclipse.xtext.util.CancelIndicator;
import org.eclipse.xtext.util.Files;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.util.UriUtil;
import org.eclipse.xtext.validation.CheckMode;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.workspace.FileProjectConfig;
import org.eclipse.xtext.workspace.ProjectConfigAdapter;
import org.eclipse.xtext.xbase.compiler.GeneratorConfig;
import org.eclipse.xtext.xbase.compiler.GeneratorConfigProvider;
import org.eclipse.xtext.xbase.compiler.IGeneratorConfigProvider;
import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.resource.BatchLinkableResource;

import io.sarl.lang.SARLConfig;
import io.sarl.lang.compiler.GeneratorConfig2;
import io.sarl.lang.compiler.GeneratorConfigProvider2;
import io.sarl.lang.compiler.IGeneratorConfigProvider2;
import io.sarl.lang.compiler.batch.InternalXtextLogger.InternalXtextLoggerFactory;
import io.sarl.lang.extralanguage.IExtraLanguageContribution;
import io.sarl.lang.extralanguage.IExtraLanguageContributions;
import io.sarl.lang.util.Utils;
import io.sarl.lang.validation.IConfigurableIssueSeveritiesProvider;

/** The compiler from SARL that could be used for batch tasks (Maven, CLI).
 *
 * <p>This compiler is inspired by the Xtend batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public class SarlBatchCompiler {

	private static final String BINCLASS_FOLDER_PREFIX = "classes"; //$NON-NLS-1$

	private static final String STUB_FOLDER_PREFIX = "stubs"; //$NON-NLS-1$

	private static final String INTERNAL_ERROR_CODE = SarlBatchCompiler.class.getName() + ".internal_error"; //$NON-NLS-1$

	private static final Predicate<IExtraLanguageContribution> DISABLER = it -> false;

	/** The provider of resource sets.
	 */
	protected Provider<ResourceSet> resourceSetProvider;

	private File outputPath;

	private File classOutputPath;

	private File tempPath;

	private CleaningPolicy cleaningPolicy = CleaningPolicy.getDefault();

	private List<File> classpath;

	private List<File> modulepath;

	private String encoding;

	private boolean writeTraceFiles = true;

	private boolean writeStorageFiles = true;

	private boolean verbose;

	private boolean enableSarlCompilation = true;

	private boolean enableJavaPostCompilation;

	private List<File> sourcePath;

	private boolean useCurrentClassLoaderAsParent;

	private org.eclipse.emf.common.util.URI baseUri;

	private FileProjectConfig projectConfig;

	private Map<String, OutputConfiguration> outputConfigurations;

	private ClassLoader currentClassLoader;

	private ClassLoader jvmTypesClassLoader;

	private ClassLoader annotationProcessingClassLoader;

	@Inject
	private IGeneratorConfigProvider generatorConfigProvider;

	@Inject
	private IGeneratorConfigProvider2 generatorConfigProvider2;

	@Inject
	private IOutputConfigurationProvider outputConfigurationProvider;

	@Inject
	private CompilerPhases compilerPhases;

	@Inject
	private Provider<JavaIoFileSystemAccess> javaIoFileSystemAccessProvider;

	@Inject
	private IndexedJvmTypeAccess indexedJvmTypeAccess;

	@Inject
	private IEncodingProvider.Runtime encodingProvider;

	@Inject
	private FileExtensionProvider fileExtensionProvider;

	@Inject
	private IResourceDescription.Manager resourceDescriptionManager;

	@Inject
	private IStubGenerator stubGenerator;

	@Inject
	private GeneratorDelegate generator;

	@Inject
	private IConfigurableIssueSeveritiesProvider issueSeverityProvider;

	@Inject
	private IExtraLanguageContributions extraLanguageContributions;

	@Inject
	@Named(Constants.LANGUAGE_NAME)
	private String languageName;

	private IJavaBatchCompiler javaCompiler;

	private Logger logger;

	private IssueMessageFormatter messageFormatter;

	private Collection<IssueMessageListener> messageListeners = new LinkedList<>();

	private Collection<ICompilatedResourceReceiver> resourceReceivers = new LinkedList<>();

	private final List<File> tempFolders = new ArrayList<>();

	private Comparator<Issue> issueComparator = new DefaultIssueComparator();

	private GeneratorConfig currentGeneratorConfiguration;

	private GeneratorConfig2 currentGeneratorConfiguration2;

	private String enabledExtraLanguageContributions;

	private boolean reportInternalProblemsAsIssues;

	private boolean reportWarningsAsErrors;

	private OptimizationLevel optimizationLevel;

	/** Constructor the batch compiler.
	 */
	public SarlBatchCompiler() {
		//
	}

	/** Change the Java compiler.
	 *
	 * @param compiler the Java compiler
	 * @since 0.8
	 */
	@Inject
	public void setJavaCompiler(IJavaBatchCompiler compiler) {
		assert compiler != null;
		this.javaCompiler = compiler;
	}

	/** Replies the Java compiler.
	 *
	 * @return the Java compiler
	 * @since 0.8
	 */
	public IJavaBatchCompiler getJavaCompiler() {
		if (this.javaCompiler == null) {
			this.javaCompiler = SarlBatchCompilerUtils.newDefaultJavaBatchCompiler();
		}
		return this.javaCompiler;
	}

	/** Change the optimization level that should be applied to the generated Java byte code.
	 *
	 * @param level the optimization level.
	 * @since 0.8
	 */
	public void setOptimizationLevel(OptimizationLevel level) {
		this.optimizationLevel = level;
	}

	/** Replies the optimization level that should be applied to the generated Java byte code.
	 *
	 * @return the optimization level.
	 * @since 0.8
	 */
	public OptimizationLevel getOptimizationLevel() {
		if (this.optimizationLevel == null) {
			this.optimizationLevel = OptimizationLevel.getDefault();
		}
		return this.optimizationLevel;
	}

	/** Change the flag that permits to report the compiler's internal problems as issues.
	 *
	 * @param reportAsIssues {@code true} if the internal errors are reported as issues.
	 * @since 0.8
	 * @see #addIssueMessageListener(IssueMessageListener)
	 */
	public void setReportInternalProblemsAsIssues(boolean reportAsIssues) {
		this.reportInternalProblemsAsIssues = reportAsIssues;
	}

	/** Replies the flag that indicates to report the compiler's internal problems as issues.
	 *
	 * @return {@code true} if the internal errors are reported as issues.
	 * @since 0.8
	 * @see #addIssueMessageListener(IssueMessageListener)
	 */
	public boolean getReportInternalProblemsAsIssues() {
		return this.reportInternalProblemsAsIssues;
	}

	/** Change the flag that permits to report the warning issues detected by the SARL compiler as errors to the user
	 * of the batch compiler.
	 *
	 * @param reportAsErrors {@code true} if the warnings are reported as errors.
	 * @since 0.13
	 * @see #addIssueMessageListener(IssueMessageListener)
	 */
	public void setReportWarningsAsErrors(boolean reportAsErrors) {
		this.reportWarningsAsErrors = reportAsErrors;
	}

	/** Replies the flag that permits to report the warning issues detected by the SARL compiler as errors to the user
	 * of the batch compiler.
	 *
	 * @return {@code true} if the warnings are reported as errors.
	 * @since 0.13
	 * @see #addIssueMessageListener(IssueMessageListener)
	 */
	public boolean getReportWarningsAsErrors() {
		return this.reportWarningsAsErrors;
	}

	/** Change the extra languages' generators that should be enabled.
	 *
	 * @param identifiers the identifier, the identifiers (separated by {@link File#pathSeparator} of the
	 *     extra languages' generator(s) to be enabled. If this parameter is {@code null}, all the extra
	 *     languages' generator are disabled.
	 * @since 0.8
	 */
	public void setExtraLanguageGenerators(String identifiers) {
		this.enabledExtraLanguageContributions = Strings.emptyIfNull(identifiers);
	}

	/** Replies the extra languages' generators that should be enabled.
	 *
	 * @return the identifier, the identifiers (separated by {@link File#pathSeparator} of the
	 *     extra languages' generator(s) to be enabled. If this parameter is {@code null}, all the extra
	 *     languages' generator are disabled.
	 * @since 0.8
	 */
	public String getExtraLanguageGenerators() {
		return this.enabledExtraLanguageContributions;
	}

	/** Set the comparator of issues that is used for sorting the issues before they are logged.
	 *
	 * @param comparator the comparator; never {@code null}.
	 */
	public void setIssueComparator(Comparator<Issue> comparator) {
		if (comparator != null) {
			this.issueComparator = comparator;
		}
	}

	/** Replies the comparator of issues that is used for sorting the issues before they are logged.
	 *
	 * @return the comparator; never {@code null}.
	 */
	public Comparator<Issue> getIssueComparator() {
		return this.issueComparator;
	}

	/** Replies if the Java compiler should be invoked after the SARL compiler is invoked.
	 *
	 * @return {@code true} if the Java compiler is invoked after the SARL compiler.
	 */
	public boolean isJavaPostCompilationEnable() {
		return this.enableJavaPostCompilation;
	}

	/** Set if the Java compiler should be invoked after the SARL compiler is invoked.
	 *
	 * @param enable {@code true} if the Java compiler is invoked after the SARL compiler.
	 */
	public void setJavaPostCompilationEnable(boolean enable) {
		this.enableJavaPostCompilation = enable;
	}

	/** Replies if the SARL compiler should be invoked before the Java compiler is invoked.
	 *
	 * @return {@code true} if the SARL compiler is invoked before the Java compiler.
	 * @since 0.12
	 */
	public boolean isSarlCompilationEnable() {
		return this.enableSarlCompilation;
	}

	/** Set if the SARL compiler should be invoked before the Java compiler is invoked.
	 *
	 * @param enable is {@code true} if the SARL compiler is invoked before the Java compiler.
	 * @since 0.12
	 */
	public void setSarlCompilationEnable(boolean enable) {
		this.enableSarlCompilation = enable;
	}

	/** Replies the formatter of the issue messages.
	 *
	 * @return the formatter.
	 */
	public IssueMessageFormatter getIssueMessageFormatter() {
		return this.messageFormatter;
	}

	/** Set the formatter of the issue messages.
	 *
	 * @param formatter the formatter.
	 */
	public void setIssueMessageFormatter(IssueMessageFormatter formatter) {
		this.messageFormatter = formatter;
	}

	/** Add a listener on the issue messages.
	 *
	 * @param listener the listener.
	 * @since 0.6
	 */
	public void addIssueMessageListener(IssueMessageListener listener) {
		this.messageListeners.add(listener);
	}

	/** Add a listener on the issue messages.
	 *
	 * @param listener the listener.
	 * @since 0.6
	 */
	public void removeIssueMessageListener(IssueMessageListener listener) {
		this.messageListeners.remove(listener);
	}

	/** Replies the message for the given issue.
	 *
	 * @param concreteSeverity the severity that was considered by the batch compiler. It may be stronger alert level that those in the {@code issue}.
	 * @param issue the issue.
	 * @param uri URI to the problem.
	 * @param message the formatted message.
	 * @since 0.6
	 */
	private void notifiesIssueMessageListeners(Severity concreteSeverity, Issue issue, org.eclipse.emf.common.util.URI uri, String message) {
		for (final var listener : this.messageListeners) {
			listener.onIssue(concreteSeverity, issue, uri, message);
		}
	}

	/** Add a receiver on the successfully compiled resources.
	 *
	 * @param receiver the receiver.
	 * @since 0.6
	 */
	public void addCompiledResourceReceiver(ICompilatedResourceReceiver receiver) {
		this.resourceReceivers.add(receiver);
	}

	/** Remove a receiver on the successfully compiled resources.
	 *
	 * @param receiver the receiver.
	 * @since 0.6
	 */
	public void removeCompiledResourceReceiver(ICompilatedResourceReceiver receiver) {
		this.resourceReceivers.remove(receiver);
	}

	/** Replies the message for the given issue.
	 *
	 * @param resource the compiled resource.
	 * @since 0.6
	 */
	private void notifiesCompiledResourceReceiver(Resource resource) {
		for (final var receiver : this.resourceReceivers) {
			receiver.receiveCompiledResource(resource);
		}
	}

	/** Replies the logger.
	 *
	 * @return the logger.
	 */
	public Logger getLogger() {
		if (this.logger == null) {
			this.logger = Logger.getLogger(getClass().getName());
		}
		return this.logger;
	}

	/** Set the logger.
	 *
	 * @param logger the logger.
	 */
	public void setLogger(Logger logger) {
		this.logger = logger;
	}

	/** Set the provider of resource sets.
	 *
	 * @param resourceSetProvider the provider.
	 */
	@Inject
	public void setResourceSetProvider(Provider<ResourceSet> resourceSetProvider) {
		this.resourceSetProvider = resourceSetProvider;
	}

	private static File normalizeFile(String file) {
		return new File(new File(file).getAbsoluteFile().toURI().normalize());
	}

	/** Replies if the trace files must be generated.
	 *
	 * <p>A trace file contains the links between the class, java and SARL files.
	 * They are mandatory for retrieving and displaying the SARL source code from
	 * a JVM element.
	 *
	 * <p>The usual filename for the trace files follows the pattern
	 * {@code .Type.java._trace}, where {@code Type} is the name of the SARL type declaration.
	 *
	 * @return {@code true} for generation.
	 */
	public boolean isWriteTraceFiles() {
		return this.writeTraceFiles;
	}

	/** Set if the trace files must be generated.
	 *
	 * <p>A trace file contains the links between the class, java and SARL files.
	 * They are mandatory for retreiving and displaying the SARL source code from
	 * a JVM element.
	 *
	 * <p>The usual filename for the trace files follows the pattern
	 * {@code .Type.java._trace}, where {@code Type} is the name of the SARL type declaration.
	 *
	 * @param writeTraceFiles {@code true} for generation.
	 */
	public void setWriteTraceFiles(boolean writeTraceFiles) {
		this.writeTraceFiles = writeTraceFiles;
	}

	/** Replies if the storage files must be generated.
	 *
	 * <p>The storage files are binary versions of the resources in order
	 * to have faster reading/accessing.
	 *
	 * <p>The usual filename for the storage files follows the pattern
	 * {@code .Type.sarlbin}, where {@code Type} is the name of the SARL type declaration.
	 *
	 * @return {@code true} for generation.
	 */
	@Pure
	public boolean isWriteStorageFiles() {
		return this.writeStorageFiles;
	}

	/** Set if the storage files must be generated.
	 *
	 * <p>The storage files are binary versions of the resources in order
	 * to have faster reading/accessing.
	 *
	 * <p>The usual filename for the storage files follows the pattern
	 * {@code .Type.sarlbin}, where {@code Type} is the name of the SARL type declaration.
	 *
	 * @param writeStorageFiles {@code true} for generation.
	 */
	public void setWriteStorageFiles(boolean writeStorageFiles) {
		this.writeStorageFiles = writeStorageFiles;
	}

	/** Replies if the compiler is verbose.
	 *
	 * @return {@code true} if the compiler is verbose.
	 */
	@Pure
	public boolean isJavaCompilerVerbose() {
		return this.verbose;
	}

	/** Set the underlying Java compiler verbosity.
	 *
	 * @param verbose {@code true} if the Java compiler is verbose.
	 */
	public void setJavaCompilerVerbose(boolean verbose) {
		this.verbose = verbose;
	}

	/** Replies the current class loader.
	 *
	 * @return the class loader.
	 */
	@Pure
	public ClassLoader getCurrentClassLoader() {
		if (this.currentClassLoader == null) {
			this.currentClassLoader = getClass().getClassLoader();
		}
		return this.currentClassLoader;
	}

	/** Set the current class loader.
	 *
	 * @param loader the new current class loader.
	 */
	public void setCurrentClassLoader(ClassLoader loader) {
		this.currentClassLoader = null;
	}

	/** Set if the class loaderr of this batch compiler must be used as sthe parent class loader.
	 *
	 * @param useCurrentClassLoaderAsParent {@code true} for using the class loader of this batch compiler.
	 */
	public void setUseCurrentClassLoaderAsParent(boolean useCurrentClassLoaderAsParent) {
		this.useCurrentClassLoaderAsParent = useCurrentClassLoaderAsParent;
	}

	/** Replies if the class loaderr of this batch compiler must be used as sthe parent class loader.
	 *
	 * @return {@code true} for using the class loader of this batch compiler.
	 */
	@Pure
	public boolean isUseCurrentClassLoaderAsParent() {
		return this.useCurrentClassLoaderAsParent;
	}

	/** Change the base path.
	 *
	 * @param basePath the base path.
	 */
	public void setBasePath(String basePath) {
		setBaseURI(UriUtil.createFolderURI(normalizeFile(basePath)));
	}

	/** Change the base URI.
	 *
	 * @param basePath the base path.
	 */
	public void setBaseURI(org.eclipse.emf.common.util.URI basePath) {
		this.baseUri = basePath;
	}

	/** Change the path where the Java files are generated.
	 *
	 * @param path the path, or {@code null} for using the default path in {@link SARLConfig#FOLDER_SOURCE_GENERATED}..
	 */
	public void setOutputPath(File path) {
		this.outputPath = path;
	}

	/** Change the path where the Java files are generated.
	 *
	 * @param path the path.
	 */
	public void setOutputPath(String path) {
		setOutputPath(normalizeFile(path));
	}

	/** Replies the path where the Java files are generated.
	 *
	 * @return the path; or {@code null} for using the default path in {@link SARLConfig#FOLDER_SOURCE_GENERATED}.
	 */
	@Pure
	public File getOutputPath() {
		return this.outputPath;
	}

	/** Replies the path where the class files are generated.
	 *
	 * @return the path; or {@code null} for ignoring the class generation.
	 */
	@Pure
	public File getClassOutputPath() {
		return this.classOutputPath;
	}

	/** Set the path where the class files are generated.
	 *
	 * @param path the path; or {@code null} for ignoring the class generation.
	 */
	@Pure
	public void setClassOutputPath(File path) {
		this.classOutputPath = path;
	}

	/** Change the classpath.
	 *
	 * <p>The classpath is a list the names of folders or jar files that are separated by {@link File#pathSeparator}.
	 *
	 * @param classpath the new classpath.
	 */
	public void setClassPath(String classpath) {
		this.classpath = new ArrayList<>();
		for (final var path : Strings.split(classpath, File.pathSeparator)) {
			this.classpath.add(normalizeFile(path));
		}
	}

	/** Change the classpath.
	 *
	 * @param classpath the new classpath.
	 */
	public void setClassPath(Collection<File> classpath) {
		this.classpath = new ArrayList<>(classpath);
	}

	/** Replies the classpath.
	 *
	 * @return the classpath.
	 */
	@Pure
	public List<File> getClassPath() {
		if (this.classpath == null) {
			return Collections.emptyList();
		}
		return Collections.unmodifiableList(this.classpath);
	}

	/** Change the module-path.
	 * This function does nothing if the current version of Java is not supporting modules.
	 *
	 * <p>The module-path is a list the names of folders or jar files that are separated by {@link File#pathSeparator}.
	 *
	 * @param modulepath the new module-path.
	 * @since 0.12
	 */
	public void setModulePath(String modulepath) {
		this.modulepath = new ArrayList<>();
		for (final var path : Strings.split(modulepath, File.pathSeparator)) {
			this.modulepath.add(normalizeFile(path));
		}
	}

	/** Change the module-path.
	 * This function does nothing if the current version of Java is not supporting modules.
	 *
	 * @param modulepath the new module-path.
	 * @since 0.12
	 */
	public void setModulePath(Collection<File> modulepath) {
		this.modulepath = new ArrayList<>(modulepath);
	}

	/** Replies the module-path.
	 * This function replies the empty list if the current version of Java is not supporting modules.
	 *
	 * @return the module-path.
	 * @since 0.12
	 */
	@Pure
	public List<File> getModulePath() {
		if (this.modulepath == null) {
			return Collections.emptyList();
		}
		return Collections.unmodifiableList(this.modulepath);
	}

	/** Change the path where the Xtext stubs are generated.
	 *
	 * @param path the path.
	 */
	public void setTempDirectory(File path) {
		this.tempPath = path;
	}

	/** Change the path where the Xtext stubs are generated.
	 *
	 * @param path the path.
	 */
	public void setTempDirectory(String path) {
		setTempDirectory(normalizeFile(path));
	}

	/** Replies the path where the Xtext stubs are generated.
	 *
	 * @return the path; or {@code null} for using the default path.
	 */
	@Pure
	public File getTempDirectory() {
		if (this.tempPath == null) {
			this.tempPath = createTempDirectory();
		}
		return this.tempPath;
	}

	/** Create the temp directory that should be used by the compiler.
	 *
	 * @return the temp directory, never {@code null}.
	 */
	@SuppressWarnings("static-method")
	protected File createTempDirectory() {
		final var tmpPath = new File(System.getProperty("java.io.tmpdir")); //$NON-NLS-1$
		var i = 0;
		var tmp = new File(tmpPath, "sarlc" + i); //$NON-NLS-1$
		while (tmp.exists()) {
			++i;
			tmp = new File(tmpPath, "sarlc" + i); //$NON-NLS-1$
		}
		return tmp;
	}

	/** Replies the cleaning policy that is applied by this batch compiler.
	 *
	 * @return the cleaning policy, never {@code null}.
	 * @since 0.10
	 */
	@Pure
	public CleaningPolicy getCleaningPolicy() {
		return this.cleaningPolicy;
	}

	/** Change the cleaning policy that is applied by this batch compiler.
	 *
	 * @param policy the cleaning policy. If it is {@code null}, the
	 *     {@link CleaningPolicy#getDefault() default policy} is used.
	 * @since 0.10
	 */
	public void setCleaningPolicy(CleaningPolicy policy) {
		this.cleaningPolicy = policy == null ? CleaningPolicy.getDefault() : policy;
	}

	/** Replies if the temp folder must be deleted at the end of the compilation.
	 *
	 * @return {@code true} if the temp folder is deleted.
	 * @deprecated since 0.10, see {@link #getCleaningPolicy()}.
	 */
	@Pure
	@Deprecated(forRemoval = true, since = "0.10")
	@Inline(value = "getCleaningPolicy() != $1.NO_CLEANING", imported = {CleaningPolicy.class})
	public boolean isDeleteTempDirectory() {
		return getCleaningPolicy() != CleaningPolicy.NO_CLEANING;
	}

	/** Set if the temp folder must be deleted at the end of the compilation.
	 *
	 * @param delete {@code true} if the temp folder is deleted.
	 * @deprecated since 0.10, see {@link #setCleaningPolicy(CleaningPolicy)}.
	 */
	@Deprecated(forRemoval = true, since = "0.10")
	@Inline(value = "setCleaningPolicy(($1) ? $2.INTERNAL_CLEANING : $2.NO_CLEANING)", imported = {CleaningPolicy.class})
	public void setDeleteTempDirectory(boolean delete) {
		setCleaningPolicy(delete ? CleaningPolicy.INTERNAL_CLEANING : CleaningPolicy.NO_CLEANING);
	}

	/** Change the file encoding.
	 *
	 * @param encoding the encoding, usually {@code UTF-8}.
	 */
	public void setFileEncoding(String encoding) {
		this.encoding = encoding;
	}

	/** Change the file encoding.
	 *
	 * @return the file encoding, or {@code null} if the default encoding must be used.
	 */
	@Pure
	public String getFileEncoding() {
		return this.encoding;
	}

	/** Replies the current generator config.
	 *
	 * @return the generator config.
	 */
	protected GeneratorConfig getGeneratorConfig() {
		if (this.currentGeneratorConfiguration == null) {
			this.currentGeneratorConfiguration = this.generatorConfigProvider.get(null);
		}
		return this.currentGeneratorConfiguration;
	}

	/** Replies the current generator config v2.
	 *
	 * @return the generator config v2.
	 */
	protected GeneratorConfig2 getGeneratorConfig2() {
		if (this.currentGeneratorConfiguration2 == null) {
			this.currentGeneratorConfiguration2 = this.generatorConfigProvider2.get(null);
		}
		return this.currentGeneratorConfiguration2;
	}

	/** Change the version of the Java source to be used for the generated Java files.
	 *
	 * @param version the Java version.
	 */
	public void setJavaSourceVersion(String version) {
		final var javaVersion = JavaVersion.fromQualifier(version);
		if (javaVersion == null) {
			final var qualifiers = new ArrayList<String>();
			for (final var vers : JavaVersion.values()) {
				qualifiers.addAll(vers.getAllQualifiers());
			}

			throw new RuntimeException(MessageFormat.format(
					Messages.SarlBatchCompiler_0, version, Joiner.on(Messages.SarlBatchCompiler_1).join(qualifiers)));
		}
		getGeneratorConfig().setJavaSourceVersion(javaVersion);
	}

	/** Replies the version of the Java source to be used for the generated Java files.
	 *
	 * @return the Java version.
	 */
	@Pure
	public String getJavaSourceVersion() {
		return getGeneratorConfig().getJavaSourceVersion().getQualifier();
	}

	/** Replies if the current project is modular.
	 * A project is module when it defines the "module-info.java" file.
	 *
	 * @return {@code true} if the project is detected as modular.
	 * @since 0.12
	 */
	@Pure
	public boolean isModularProject() {
		for (final var folder : getSourcePaths()) {
			final var infoFile = new File(folder, "module-info.java"); //$NON-NLS-1$
			if (infoFile.isFile()) {
				return true;
			}
		}
		return false;
	}

	/** Replies the compiler generate the Xbase expressions.
	 *
	 * @return {@code true} if the compiler generates the expressions
	 */
	@Pure
	public boolean isGenerateExpressions() {
		return getGeneratorConfig().isGenerateExpressions();
	}

	/** Set if the compiler generate the Xbase expressions.
	 *
	 * @param generateExpressions {@code true} if the compiler generates the expressions
	 */
	public void setGenerateExpressions(boolean generateExpressions) {
		getGeneratorConfig().setGenerateExpressions(generateExpressions);
	}

	/** Replies the {@code @SuppressWarnings} is generated.
	 *
	 * @return {@code true} if the compiler generates the warning supression annotations.
	 */
	@Pure
	public boolean isGenerateSyntheticSuppressWarnings() {
		return getGeneratorConfig().isGenerateSyntheticSuppressWarnings();
	}

	/** Set if the {@code @SuppressWarnings} is generated.
	 *
	 * @param generateAnnotations {@code true} if the compiler generates the warning supression annotations.
	 */
	public void setGenerateSyntheticSuppressWarnings(boolean generateAnnotations) {
		getGeneratorConfig().setGenerateSyntheticSuppressWarnings(generateAnnotations);
	}

	/** Replies the {@code @Generated} is generated.
	 *
	 * @return {@code true} if the compiler generates the generated annotations.
	 */
	@Pure
	public boolean isGenerateGeneratedAnnotation() {
		return getGeneratorConfig().isGenerateGeneratedAnnotation();
	}

	/** Set if the {@code @Generated} is generated.
	 *
	 * @param generateAnnotations {@code true} if the compiler generates the generated annotations.
	 */
	public void setGenerateGeneratedAnnotation(boolean generateAnnotations) {
		getGeneratorConfig().setGenerateGeneratedAnnotation(generateAnnotations);
	}

	/** Replies if the generation date is included in the {@code @Generated} annotations.
	 *
	 * @return {@code true} if the generation date is added.
	 */
	@Pure
	public boolean isIncludeDateInGeneratedAnnotation() {
		return getGeneratorConfig().isIncludeDateInGeneratedAnnotation();
	}

	/** Set if the generation date is included in the {@code @Generated} annotations.
	 *
	 * @param includeDateInGeneratedAnnotation {@code true} if the generation date is added.
	 */
	public void setIncludeDateInGeneratedAnnotation(boolean includeDateInGeneratedAnnotation) {
		getGeneratorConfig().setIncludeDateInGeneratedAnnotation(includeDateInGeneratedAnnotation);
	}

	/** Replies the comment in the {@code @Generated} annnotations.
	 *
	 * @return the comment.
	 */
	@Pure
	public String getGeneratedAnnotationComment() {
		return getGeneratorConfig().getGeneratedAnnotationComment();
	}

	/** Set the comment in the {@code @Generated} annnotations.
	 *
	 * @param comment the comment.
	 */
	public void setGeneratedAnnotationComment(String comment) {
		getGeneratorConfig().setGeneratedAnnotationComment(comment);
	}

	/** Replies if the {@code @Inline} shall be generated.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isGenerateInlineAnnotation() {
		return getGeneratorConfig2().isGenerateInlineAnnotation();
	}

	/** Set if the {@code @Inline} shall be generated.
	 *
	 * @param generateInlineAnnotation {@code true} if annotation shall be generated.
	 */
	public void setGenerateInlineAnnotation(final boolean generateInlineAnnotation) {
		getGeneratorConfig2().setGenerateInlineAnnotation(generateInlineAnnotation);
	}

	/** Replies if constant expression interpreter shall be called for generated {@code @Inline}.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isUseExpressionInterpreterForInlineAnnotation() {
		return getGeneratorConfig2().isUseExpressionInterpreterForInlineAnnotation();
	}

	/** Set if the constant expression interpreter shall be called for generated {@code @Inline}.
	 *
	 * @param generateInlineAnnotation {@code true} if annotation shall be generated.
	 */
	public void setUseExpressionInterpreterForInlineAnnotation(final boolean generateInlineAnnotation) {
		getGeneratorConfig2().setUseExpressionInterpreterForInlineAnnotation(generateInlineAnnotation);
	}

	/** Replies if the {@code @Pure} shall be generated.
	 *
	 * @return {@code true} if annotation shall be generated.
	 */
	@Pure
	public boolean isGeneratePureAnnotation() {
		return getGeneratorConfig2().isGeneratePureAnnotation();
	}

	/** Set if the {@code @Pure} shall be generated.
	 *
	 * @param generatePureAnnotation {@code true} if annotation shall be generated.
	 */
	public void setGeneratePureAnnotation(final boolean generatePureAnnotation) {
		getGeneratorConfig2().setGeneratePureAnnotation(generatePureAnnotation);
	}

	/** Replies if the equality test functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateEqualityTestFunctions() {
		return getGeneratorConfig2().isGenerateEqualityTestFunctions();
	}

	/** Set if the equality test functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateEqualityTestFunctions(final boolean generateFunctions) {
		getGeneratorConfig2().setGenerateEqualityTestFunctions(generateFunctions);
	}

	/** Replies if the toString functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateToStringFunctions() {
		return getGeneratorConfig2().isGenerateToStringFunctions();
	}

	/** Set if the toString functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateToStringFunctions(final boolean generateFunctions) {
		getGeneratorConfig2().setGenerateToStringFunctions(generateFunctions);
	}

	/** Replies if the clone functions shall be generated.
	 *
	 * @return {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateCloneFunctions() {
		return getGeneratorConfig2().isGenerateCloneFunctions();
	}

	/** Set if the clone functions shall be generated.
	 *
	 * @param generateFunctions {@code true} if the functions shall be generated.
	 * @since 0.8
	 */
	public void setGenerateCloneFunctions(final boolean generateFunctions) {
		getGeneratorConfig2().setGenerateCloneFunctions(generateFunctions);
	}

	/** Replies if the serial number fields shall be generated.
	 *
	 * @return {@code true} if the fields shall be generated.
	 * @since 0.8
	 */
	@Pure
	public boolean isGenerateSerialNumberFields() {
		return getGeneratorConfig2().isGenerateSerialNumberFields();
	}

	/** Set if the serial number fields shall be generated.
	 *
	 * @param generateFields {@code true} if the fields shall be generated.
	 * @since 0.8
	 */
	public void setGenerateSerialNumberFields(final boolean generateFields) {
		getGeneratorConfig2().setGenerateSerialNumberFields(generateFields);
	}

	/** Change the source path.
	 *
	 * <p>The source path is a list the names of folders that are separated by {@link File#pathSeparator}.
	 *
	 * @param sourcePath the new source path.
	 */
	public void setSourcePath(String sourcePath) {
		this.sourcePath = new ArrayList<>();
		for (final var path : Strings.split(sourcePath, File.pathSeparator)) {
			this.sourcePath.add(normalizeFile(path));
		}
	}

	/** Change the source path.
	 *
	 * @param sourcePath the new source path.
	 */
	public void setSourcePath(Collection<File> sourcePath) {
		this.sourcePath = new ArrayList<>(sourcePath);
	}

	/** Add a folder to the source path.
	 *
	 * @param sourcePath the new source path.
	 */
	public void addSourcePath(String sourcePath) {
		if (!Strings.isEmpty(sourcePath)) {
			addSourcePath(normalizeFile(sourcePath));
		}
	}

	/** Add a folder to the source path.
	 *
	 * @param sourcePath the new source path.
	 */
	public void addSourcePath(File sourcePath) {
		if (this.sourcePath == null) {
			this.sourcePath = new ArrayList<>();
		}
		this.sourcePath.add(sourcePath);
	}

	/** Replies the source path.
	 *
	 * @return the source path.
	 */
	@Pure
	public List<File> getSourcePaths() {
		if (this.sourcePath == null) {
			return Collections.emptyList();
		}
		return Collections.unmodifiableList(this.sourcePath);
	}

	private List<String> getSourcePathStrings() {
		if (this.sourcePath == null) {
			return Collections.emptyList();
		}
		final var list = new ArrayList<String>(this.sourcePath.size());
		for (final var input : this.sourcePath) {
			list.add(input.getAbsolutePath());
		}
		return list;
	}

	private void configureExtraLanguageGenerators() {
		final var generators = getExtraLanguageGenerators();
		if (Strings.isEmpty(generators)) {
			this.extraLanguageContributions.setContributionChecker(DISABLER);
		} else {
			final var identifiers = generators.split("\\s*" + Pattern.quote(File.pathSeparator) + "\\s*"); //$NON-NLS-1$ //$NON-NLS-2$
			this.extraLanguageContributions.setContributionChecker(it -> {
				for (final var id : identifiers) {
					if (it.isAcceptedIdentifier(id)) {
						return true;
					}
				}
				return false;
			});
		}
	}

	private void unconfigureExtraLanguageGenerators() {
		this.extraLanguageContributions.setContributionChecker(null);
	}

	/** Run the compilation.
	 *
	 * @return success status.
	 */
	@Inline(value = "compile(($1) null)", imported = {IProgressMonitor.class})
	public boolean compile() {
		return compile((IProgressMonitor) null);
	}

	/** Run the compilation.
	 *
	 * @param cancel is the tool for canceling the compilation.
	 * @return success status.
	 */
	public boolean compile(CancelIndicator cancel) {
		return compile(cancel != null ? new CancelIndicatorProgressMonitor(cancel) : null);
	}

	/** Run the compilation.
	 *
	 * @param progress monitor of the progress of the compilation.
	 * @return success status.
	 * @since 0.8
	 */
	public boolean compile(IProgressMonitor progress) {
		final var monitor = progress == null ? new NullProgressMonitor() : progress;
		try {
			monitor.beginTask(Messages.SarlBatchCompiler_42, 18);
			if (!checkConfiguration(monitor)) {
				return false;
			}
			monitor.worked(1);
			final var resourceSet = this.resourceSetProvider.get();
			configureExtraLanguageGenerators();
			if (!configureWorkspace(resourceSet, monitor)) {
				return false;
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(Utils.dump(getGeneratorConfig(), false));
			}
			monitor.worked(2);
			monitor.subTask(Messages.SarlBatchCompiler_43);
			if (this.generatorConfigProvider instanceof GeneratorConfigProvider igen) {
				igen.install(resourceSet, getGeneratorConfig());
			}
			if (monitor.isCanceled()) {
				return false;
			}
			if (this.generatorConfigProvider2 instanceof GeneratorConfigProvider2 igen) {
				igen.install(resourceSet, getGeneratorConfig2());
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(Utils.dump(getGeneratorConfig2(), false));
			}
			if (monitor.isCanceled()) {
				return false;
			}
			monitor.worked(3);
			monitor.subTask(Messages.SarlBatchCompiler_44);
			final var stubClassDirectory = createTempDir(BINCLASS_FOLDER_PREFIX);
			if (monitor.isCanceled()) {
				return false;
			}
			if (isSarlCompilationEnable()) {
				monitor.worked(4);
				try {
					monitor.subTask(Messages.SarlBatchCompiler_45);
					this.compilerPhases.setIndexing(resourceSet, true);
					if (monitor.isCanceled()) {
						return false;
					}
					monitor.worked(5);
					// install a type provider without index lookup for the first phase
					installJvmTypeProvider(resourceSet, stubClassDirectory, true, monitor);
					if (monitor.isCanceled()) {
						return false;
					}
					monitor.worked(6);
					loadSARLFiles(resourceSet, monitor);
					if (monitor.isCanceled()) {
						return false;
					}
					monitor.worked(7);
					final var stubSourceDirectory = createStubs(resourceSet, monitor);
					if (monitor.isCanceled()) {
						return false;
					}
					monitor.worked(8);
					var compilerStatus = preCompileStubs(stubSourceDirectory, stubClassDirectory, monitor);
					if (!compilerStatus.isSuccess() && compilerStatus != CompilerStatus.NOTHING_TO_COMPILE) {
						if (compilerStatus != CompilerStatus.CANCELED) {
							reportInternalError(MessageFormat.format(Messages.SarlBatchCompiler_2, compilerStatus.getFailureExplanation()));
						}
						return false;
					}
					monitor.worked(9);
					compilerStatus = preCompileJava(stubSourceDirectory, stubClassDirectory, monitor);
					if (!compilerStatus.isSuccess() && compilerStatus != CompilerStatus.NOTHING_TO_COMPILE) {
						if (compilerStatus == CompilerStatus.CANCELED) {
							return false;
						}
						if (getLogger().isLoggable(Level.FINEST)) {
							getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_3, compilerStatus.getFailureExplanation()));
						}
					}
					monitor.worked(10);
				} finally {
					monitor.subTask(Messages.SarlBatchCompiler_46);
					this.compilerPhases.setIndexing(resourceSet, false);
					if (monitor.isCanceled()) {
						return false;
					}
				}
				monitor.worked(11);
				// install a fresh type provider for the second phase, so we clear all previously cached classes and misses.
				installJvmTypeProvider(resourceSet, stubClassDirectory, false, monitor);
				if (monitor.isCanceled()) {
					return false;
				}
				monitor.worked(12);
				generateJvmElements(resourceSet, monitor);
				if (monitor.isCanceled()) {
					return false;
				}
				monitor.worked(13);
				final var validatedResources = new ArrayList<Resource>();
				final var issues = validate(resourceSet, validatedResources, monitor);
				if (monitor.isCanceled()) {
					return false;
				}
				if (!issues.isEmpty()) {
					if (reportCompilationIssues(issues)) {
						return false;
					}
				}
				monitor.worked(14);
				overrideXtextInternalLoggers();
				generateJavaFiles(validatedResources, monitor);
				if (monitor.isCanceled()) {
					return false;
				}
			}
			monitor.worked(15);
			if (isJavaPostCompilationEnable()) {
				final var compilerStatus = postCompileJava(monitor);
				if (!compilerStatus.isSuccess() && compilerStatus != CompilerStatus.NOTHING_TO_COMPILE) {
					if (compilerStatus != CompilerStatus.CANCELED) {
						reportInternalError(MessageFormat.format(Messages.SarlBatchCompiler_2, compilerStatus.getFailureExplanation()));
					}
					return false;
				}
			} else {
				reportInternalWarning(Messages.SarlBatchCompiler_65);
			}
			monitor.worked(16);
		} finally {
			finalizationStage(monitor);
			monitor.done();
		}
		return true;
	}

	private void finalizationStage(IProgressMonitor monitor) {
		monitor.subTask(Messages.SarlBatchCompiler_47);
		destroyClassLoader(this.jvmTypesClassLoader);
		destroyClassLoader(this.annotationProcessingClassLoader);
		switch (getCleaningPolicy()) {
		case FULL_CLEANING:
			if (this.tempPath != null) {
				cleanFolder(this.tempPath, null);
			}
			break;
		case NO_CLEANING:
			// Do nothing
			break;
		case INTERNAL_CLEANING:
			monitor.subTask(Messages.SarlBatchCompiler_48);
			for (final File file : this.tempFolders) {
				cleanFolder(file, null);
			}
			break;
		default:
			// This case should never occur
			throw new IllegalStateException();
		}
		this.tempPath = null;
		this.tempFolders.clear();
		//
		unconfigureExtraLanguageGenerators();
	}

	/** Change the loggers that are internally used by Xtext.
	 */
	protected void overrideXtextInternalLoggers() {
		final var logger = getLogger();
		final var factory = new InternalXtextLoggerFactory(logger);
		final var internalLogger = org.apache.log4j.Logger.getLogger(
				MessageFormat.format(Messages.SarlBatchCompiler_40, logger.getName()), factory);
		final var lr = new LoggerRepositoryWrapper(LogManager.getLoggerRepository());
		lr.registerWrapper("org.eclipse.xtext.xbase.resource.BatchLinkableResourceStorageWritable", internalLogger); //$NON-NLS-1$
		lr.registerWrapper("org.eclipse.xtext.xbase.resource.BatchLinkableResource", internalLogger); //$NON-NLS-1$
		lr.registerWrapper("org.eclipse.xtend.core.macro.ProcessorInstanceForJvmTypeProvider", internalLogger); //$NON-NLS-1$
		LogManager.setRepositorySelector(() -> lr, SarlBatchCompiler.class);
	}

	/** Create a message for the issue.
	 *
	 * @param severity the severity that is considered by the compiler. It may be stronger than the one specified in the issue.
	 * @param issue the issue.
	 * @return the message.
	 */
	protected String createIssueMessage(Severity severity, Issue issue) {
		final var formatter = getIssueMessageFormatter();
		final var uriToProblem = issue.getUriToProblem();
		if (formatter != null) {
			final var message = formatter.format(severity, issue, uriToProblem);
			if (message != null) {
				return message;
			}
		}
		if (uriToProblem != null) {
			final var resourceUri = uriToProblem.trimFragment();
			return MessageFormat.format(Messages.SarlBatchCompiler_4,
					severity, resourceUri.lastSegment(),
					resourceUri.isFile() ? resourceUri.toFileString() : "", //$NON-NLS-1$
							issue.getLineNumber(), issue.getColumn(), issue.getCode(), issue.getMessage());
		}
		return MessageFormat.format(Messages.SarlBatchCompiler_5,
				severity, issue.getLineNumber(), issue.getColumn(), issue.getCode(), issue.getMessage());
	}

	/** Output the given issues that result from the compilation of the SARL code.
	 *
	 * @param issues the issues to report.
	 * @return {@code true} if at least one error was reported, {@code false} if
	 *      no error was reported.
	 */
	protected boolean reportCompilationIssues(Iterable<Issue> issues) {
		var hasError = false;
		for (final var issue : issues) {
			final Severity concreteSeverity;
			final String issueMessage;
			switch (issue.getSeverity()) {
			case ERROR:
				hasError = true;
				concreteSeverity = Severity.ERROR;
				issueMessage = createIssueMessage(concreteSeverity, issue);
				getLogger().severe(issueMessage);
				break;
			case WARNING:
				if (getReportWarningsAsErrors()) {
					hasError = true;
					concreteSeverity = Severity.ERROR;
					issueMessage = createIssueMessage(concreteSeverity, issue);
					getLogger().severe(issueMessage);
				} else {
					concreteSeverity = Severity.WARNING;
					issueMessage = createIssueMessage(concreteSeverity, issue);
					getLogger().warning(issueMessage);
				}
				break;
			case INFO:
				concreteSeverity = Severity.INFO;
				issueMessage = createIssueMessage(concreteSeverity, issue);
				getLogger().info(issueMessage);
				break;
			case IGNORE:
			default:
				concreteSeverity = Severity.IGNORE;
				issueMessage = createIssueMessage(concreteSeverity, issue);
				break;
			}
			notifiesIssueMessageListeners(concreteSeverity, issue, issue.getUriToProblem(), issueMessage);
		}
		return hasError;
	}

	/** Reports the given warning message.
	 *
	 * @param message the warning message.
	 * @since 0.8
	 */
	protected void reportInternalWarning(String message) {
		getLogger().warning(message);
		if (getReportInternalProblemsAsIssues()) {
			final org.eclipse.emf.common.util.URI uri  = null;
			final var issue = new Issue.IssueImpl();
			issue.setCode(INTERNAL_ERROR_CODE);
			issue.setMessage(message);
			issue.setUriToProblem(uri);
			issue.setSeverity(Severity.WARNING);
			notifiesIssueMessageListeners(
					getReportWarningsAsErrors() ? Severity.ERROR : Severity.WARNING,
							issue, uri, message);
		}
	}

	/** Reports the given warning message.
	 *
	 * @param message the warning message.
	 * @param exception the source of the exception.
	 * @since 0.8
	 */
	protected void reportInternalWarning(String message, Throwable exception) {
		getLogger().log(Level.WARNING, message, exception);
		if (getReportInternalProblemsAsIssues()) {
			final org.eclipse.emf.common.util.URI uri  = null;
			final var issue = new Issue.IssueImpl();
			issue.setCode(INTERNAL_ERROR_CODE);
			issue.setMessage(message);
			issue.setUriToProblem(uri);
			issue.setSeverity(Severity.WARNING);
			notifiesIssueMessageListeners(
					getReportWarningsAsErrors() ? Severity.ERROR : Severity.WARNING,
							issue, uri, message);
		}
	}

	/** Reports the given error message.
	 *
	 * @param message the error message.
	 * @param exception the source of the exception.
	 * @since 0.8
	 */
	protected void reportInternalError(String message, Throwable exception) {
		getLogger().log(Level.SEVERE, message, exception);
		if (getReportInternalProblemsAsIssues()) {
			final org.eclipse.emf.common.util.URI uri  = null;
			final var issue = new Issue.IssueImpl();
			issue.setCode(INTERNAL_ERROR_CODE);
			issue.setMessage(message);
			issue.setUriToProblem(uri);
			issue.setSeverity(Severity.ERROR);
			notifiesIssueMessageListeners(Severity.ERROR, issue, uri, message);
		}
	}

	/** Reports the given error message.
	 *
	 * @param message the error message.
	 * @param parameters the values of the parameters that must be dynamically replaced within the message text.
	 * @since 0.8
	 */
	protected void reportInternalError(String message, Object... parameters) {
		String msg;
		Throwable error = null;
		if (parameters == null || parameters.length <= 0) {
			msg = message;
		} else {
			try {
				msg = MessageFormat.format(message, parameters);
			} catch (Throwable exception) {
				msg = message;
				error = exception;
			}
		}
		if (error != null) {
			getLogger().log(Level.SEVERE, msg, error);

		} else {
			getLogger().severe(msg);
		}
		if (getReportInternalProblemsAsIssues()) {
			final org.eclipse.emf.common.util.URI uri  = null;
			final var issue = new Issue.IssueImpl();
			issue.setCode(INTERNAL_ERROR_CODE);
			issue.setMessage(msg);
			issue.setUriToProblem(uri);
			issue.setSeverity(Severity.ERROR);
			notifiesIssueMessageListeners(Severity.ERROR, issue, uri, message);
		}
	}

	/** Reports the given information message.
	 *
	 * @param message the information message.
	 * @param parameters the values of the parameters that must be dynamically replaced within the message text.
	 * @since 0.12
	 */
	protected void reportInternalInfo(String message, Object... parameters) {
		getLogger().info(MessageFormat.format(message, parameters));
	}

	/** Generate the Java files from the SARL scripts.
	 *
	 * @param validatedResources the validatedResources for which the Java files could be generated.
	 * @param progress monitor of the progress of the compilation.
	 */
	protected void generateJavaFiles(Iterable<Resource> validatedResources, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_49);
		getLogger().info(MessageFormat.format(Messages.SarlBatchCompiler_28, getOutputPath()));
		final var javaIoFileSystemAccess = this.javaIoFileSystemAccessProvider.get();
		javaIoFileSystemAccess.setOutputConfigurations(this.outputConfigurations);
		// The function configureWorkspace should set the output paths with absolute paths.
		//javaIoFileSystemAccess.setOutputPath(getOutputPath().getAbsolutePath());
		javaIoFileSystemAccess.setWriteTrace(isWriteTraceFiles());
		if (progress.isCanceled()) {
			return;
		}

		final var context = new GeneratorContext();
		context.setCancelIndicator(() -> progress.isCanceled());
		for (final var resource : validatedResources) {
			if (progress.isCanceled()) {
				return;
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_23, resource.getURI().lastSegment()));
			}
			if (isWriteStorageFiles() && resource instanceof StorageAwareResource storageAwareResource) {
				storageAwareResource.getResourceStorageFacade().saveResource(storageAwareResource, javaIoFileSystemAccess);
			}
			if (progress.isCanceled()) {
				return;
			}
			this.generator.generate(resource, javaIoFileSystemAccess, context);
			notifiesCompiledResourceReceiver(resource);
		}
	}

	/** Generate the JVM model elements.
	 *
	 * @param progress monitor of the progress of the compilation.
	 * @param resourceSet the container of the scripts.
	 */
	protected void generateJvmElements(ResourceSet resourceSet, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_21);
		getLogger().info(Messages.SarlBatchCompiler_21);

		final var originalResources = resourceSet.getResources();
		final var toBeResolved = new ArrayList<Resource>(originalResources.size());
		for (final var resource : originalResources) {
			if (progress.isCanceled()) {
				return;
			}
			if (isSourceFile(resource)) {
				toBeResolved.add(resource);
			}
		}
		for (final var resource : toBeResolved) {
			if (progress.isCanceled()) {
				return;
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_26, resource.getURI().lastSegment()));
			}
			EcoreUtil.resolveAll(resource);
			EcoreUtil2.resolveLazyCrossReferences(resource, CancelIndicator.NullImpl);
		}
	}

	/** Generate the JVM model elements, and validate generated elements.
	 *
	 * @param resourceSet the container of the scripts.
	 * @param validResources will be filled by this function with the collection of resources that was successfully validated.
	 * @param progress monitor of the progress of the compilation.
	 * @return the list of the issues.
	 */
	protected List<Issue> validate(ResourceSet resourceSet, Collection<Resource> validResources, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_38);
		getLogger().info(Messages.SarlBatchCompiler_38);
		final var resources = new LinkedList<>(resourceSet.getResources());
		final var issuesToReturn = new ArrayList<Issue>();
		for (final var resource : resources) {
			if (progress.isCanceled()) {
				return issuesToReturn;
			}
			if (isSourceFile(resource)) {
				if (getLogger().isLoggable(Level.FINEST)) {
					getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_22, resource.getURI().lastSegment()));
				}
				final var resourceServiceProvider = IResourceServiceProvider.Registry.INSTANCE
						.getResourceServiceProvider(resource.getURI());
				if (resourceServiceProvider != null) {
					final var resourceValidator = resourceServiceProvider.getResourceValidator();
					final var result = resourceValidator.validate(resource, CheckMode.ALL, null);
					if (progress.isCanceled()) {
						return issuesToReturn;
					}
					final var issues = new TreeSet<>(getIssueComparator());
					boolean hasValidationError = false;
					for (final var issue : result) {
						if (progress.isCanceled()) {
							return issuesToReturn;
						}
						if (issue.isSyntaxError() || issue.getSeverity() == Severity.ERROR) {
							hasValidationError = true;
						}
						issues.add(issue);
					}
					if (!hasValidationError) {
						if (!issues.isEmpty()) {
							if (getLogger().isLoggable(Level.FINEST)) {
								getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_39, resource.getURI().lastSegment()));
							}
							issuesToReturn.addAll(issues);
						}
						validResources.add(resource);
					} else {
						if (getLogger().isLoggable(Level.FINEST)) {
							getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_39, resource.getURI().lastSegment()));
						}
						issuesToReturn.addAll(issues);
					}
				}
			}
		}
		return issuesToReturn;
	}

	/** Replies if the given resource is a script.
	 *
	 * @param resource the resource to test.
	 * @return {@code true} if the given resource is a script.
	 */
	@SuppressWarnings("static-method")
	protected boolean isSourceFile(Resource resource) {
		if (resource instanceof BatchLinkableResource bres) {
			return !bres.isLoadedFromStorage();
		}
		return false;
	}

	/** Compile the stub files before the compilation of the project's files.
	 *
	 * @param sourceDirectory the source directory where stubs are stored.
	 * @param classDirectory the output directory, where stub binary files should be generated.
	 * @param progress monitor of the progress of the compilation.
	 * @return the success status.
	 */
	protected CompilerStatus preCompileStubs(File sourceDirectory, File classDirectory, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_50);
		return runJavaCompiler(classDirectory, Collections.singletonList(sourceDirectory), getClassPath(),
				getModulePath(), true, false, progress);
	}

	/** Compile the java files before the compilation of the project's files.
	 *
	 * @param sourceDirectory the source directory where java files are stored.
	 * @param classDirectory the output directory, where binary files should be generated.
	 * @param progress monitor of the progress of the compilation.
	 * @return the success status.
	 */
	protected CompilerStatus preCompileJava(File sourceDirectory, File classDirectory, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_51);
		final var cp = Iterables.concat(Collections.singleton(sourceDirectory), getClassPath());
		final var mp = Collections.<File>emptyList();
		return runJavaCompiler(classDirectory, getSourcePaths(),
				cp, mp,
				false, false, progress);
	}

	/** Compile the java files after the compilation of the project's files.
	 *
	 * @param progress monitor of the progress of the compilation.
	 * @return the success status.
	 */
	protected CompilerStatus postCompileJava(IProgressMonitor progress) {
		assert progress != null;
		final var msg = MessageFormat.format(Messages.SarlBatchCompiler_25, getJavaCompiler().getName());
		progress.subTask(msg);
		getLogger().info(msg);
		final var classOutputPath = getClassOutputPath();
		if (classOutputPath == null) {
			getLogger().info(Messages.SarlBatchCompiler_24);
			return CompilerStatus.COMPILATION_SUCCESS;
		}
		final var sources = Iterables.concat(getSourcePaths(), Collections.singleton(getOutputPath()));
		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_29, toPathString(sources)));
		}
		final var classpath = getClassPath();
		final var modulepath = getModulePath();
		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_64, toPathString(classpath), toPathString(modulepath)));
		}
		return runJavaCompiler(classOutputPath, sources, classpath, modulepath, true, true, progress);
	}

	private static String toPathString(Iterable<File> files) {
		final var result = new StringBuilder();
		for (final File file : files) {
			if (result.length() > 0) {
				result.append(File.pathSeparator);
			}
			result.append(file.toString());
		}
		return result.toString();
	}

	/** Run the Java compiler.
	 *
	 * @param classDirectory the output directory.
	 * @param sourcePathDirectories the source directories.
	 * @param classPathEntries classpath entries.
	 * @param modulePathEntries classpath entries.
	 * @param enableCompilerOutput indicates if the Java compiler output is displayed.
	 * @param enableOptimization indicates if the Java compiler must applied optimization flags.
	 * @param progress monitor of the progress of the compilation.
	 * @return the success status.
	 * @see IJavaBatchCompiler
	 */
	@SuppressWarnings({ "resource" })
	protected CompilerStatus runJavaCompiler(File classDirectory, Iterable<File> sourcePathDirectories,
			Iterable<File> classPathEntries, Iterable<File> modulePathEntries, boolean enableCompilerOutput,
			boolean enableOptimization, IProgressMonitor progress) {
		var encoding = this.encodingProvider.getDefaultEncoding();
		if (Strings.isEmpty(encoding)) {
			encoding = null;
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}
		final PrintWriter outWriter;
		final PrintWriter errWriter;
		if (enableCompilerOutput) {
			outWriter = getInfoCompilerOutputWriter();
			errWriter = getErrorCompilerOutputWriter();
		} else {
			outWriter = getDebugCompilerOutputWriter();
			errWriter = getDebugCompilerOutputWriter();
		}
		if (progress.isCanceled()) {
			return CompilerStatus.CANCELED;
		}
		return getJavaCompiler().compile(
				classDirectory,
				sourcePathDirectories,
				classPathEntries,
				modulePathEntries,
				getJavaSourceVersion(),
				encoding,
				isJavaCompilerVerbose(),
				enableOptimization ? getOptimizationLevel() : null,
						outWriter,
						errWriter,
						getLogger(),
						progress);
	}

	@SuppressWarnings("resource")
	private PrintWriter getDebugCompilerOutputWriter() {
		final var debugWriter = new Writer() {
			@Override
			public void write(char[] data, int offset, int count) throws IOException {
				if (getLogger().isLoggable(Level.FINEST)) {
					final var message = String.copyValueOf(data, offset, count);
					getLogger().finest(message);
				}
			}

			@Override
			public void flush() throws IOException {
				//
			}

			@Override
			public void close() throws IOException {
				//
			}
		};
		return new PrintWriter(debugWriter);
	}

	@SuppressWarnings("resource")
	private PrintWriter getErrorCompilerOutputWriter() {
		final var debugWriter = new Writer() {
			@Override
			public void write(char[] data, int offset, int count) throws IOException {
				final var message = String.copyValueOf(data, offset, count);
				reportInternalError(message);
			}

			@Override
			public void flush() throws IOException {
				//
			}

			@Override
			public void close() throws IOException {
				//
			}
		};
		return new PrintWriter(debugWriter);
	}

	@SuppressWarnings("resource")
	private PrintWriter getInfoCompilerOutputWriter() {
		final var debugWriter = new Writer() {
			@Override
			public void write(char[] data, int offset, int count) throws IOException {
				final var message = String.copyValueOf(data, offset, count);
				reportInternalInfo(message);
			}

			@Override
			public void flush() throws IOException {
				//
			}

			@Override
			public void close() throws IOException {
				//
			}
		};
		return new PrintWriter(debugWriter);
	}

	/** Create the stubs.
	 *
	 * @param resourceSet the input resource set.
	 * @param progress monitor of the progress of the compilation.
	 * @return the folder in which the stubs are located. Replies {@code null} if the activity is canceled.
	 */
	protected File createStubs(ResourceSet resourceSet, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_53);
		final var outputDirectory = createTempDir(STUB_FOLDER_PREFIX);
		if (progress.isCanceled()) {
			return null;
		}
		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_19, outputDirectory));
		}
		final var fileSystemAccess = this.javaIoFileSystemAccessProvider.get();
		if (progress.isCanceled()) {
			return null;
		}
		fileSystemAccess.setOutputPath(outputDirectory.toString());
		final var resources = new ArrayList<>(resourceSet.getResources());
		for (final var resource : resources) {
			if (progress.isCanceled()) {
				return null;
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_20, resource.getURI()));
			}
			final var description = this.resourceDescriptionManager.getResourceDescription(resource);
			this.stubGenerator.doGenerateStubs(fileSystemAccess, description);
		}
		return outputDirectory;
	}

	/** Load the SARL files in the given resource set.
	 *
	 * @param progress monitor of the progress of the compilation.
	 * @param resourceSet the resource set to load from.
	 */
	protected void loadSARLFiles(ResourceSet resourceSet, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_54);
		this.encodingProvider.setDefaultEncoding(getFileEncoding());
		final var nameBasedFilter = new NameBasedFilter();
		nameBasedFilter.setExtension(this.fileExtensionProvider.getPrimaryFileExtension());
		final var pathTraverser = new PathTraverser();
		final var sourcePathDirectories = getSourcePathStrings();
		if (progress.isCanceled()) {
			return;
		}
		final var pathes = pathTraverser.resolvePathes(sourcePathDirectories,
				input -> nameBasedFilter.matches(input));
		if (progress.isCanceled()) {
			return;
		}
		for (final var source : pathes.keySet()) {
			for (final var uri : pathes.get(source)) {
				if (progress.isCanceled()) {
					return;
				}
				if (getLogger().isLoggable(Level.FINEST)) {
					getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_7, uri));
				}
				resourceSet.getResource(uri, true);
			}
		}
	}

	/** Create a temporary subdirectory inside the root temp directory.
	 *
	 * @param namePrefix the prefix for the folder name.
	 * @return the temp directory.
	 * @see #getTempDirectory()
	 */
	protected File createTempDir(String namePrefix) {
		final var tempDir = new File(getTempDirectory(), namePrefix);
		cleanFolder(tempDir, null);
		if (!tempDir.mkdirs()) {
			throw new RuntimeException(MessageFormat.format(Messages.SarlBatchCompiler_8, tempDir.getAbsolutePath()));
		}
		this.tempFolders.add(tempDir);
		return tempDir;
	}

	/** Clean the folders.
	 *
	 * @param parentFolder the parent folder.
	 * @param filter the file filter for the file to remove.
	 * @return the success status.
	 */
	protected boolean cleanFolder(File parentFolder, FileFilter filter) {
		try {
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_9, parentFolder.toString()));
			}
			return Files.cleanFolder(parentFolder, null, true, true);
		} catch (FileNotFoundException e) {
			return true;
		}
	}

	/** Check the compiler configuration; and logs errors.
	 *
	 * @param progress monitor of the progress of the compilation.
	 * @return success status. Replies {@code false} if the operation is canceled.
	 */
	protected boolean checkConfiguration(IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_55);
		final var output = getOutputPath();
		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_35, output));
		}
		if (output == null) {
			reportInternalError(Messages.SarlBatchCompiler_36);
			return false;
		}
		progress.subTask(Messages.SarlBatchCompiler_56);
		for (final var sourcePath : getSourcePaths()) {
			if (progress.isCanceled()) {
				return false;
			}
			try {
				if (getLogger().isLoggable(Level.FINEST)) {
					getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_37, sourcePath));
				}
				if (isContainedIn(output.getCanonicalFile(), sourcePath.getCanonicalFile())) {
					reportInternalError(Messages.SarlBatchCompiler_10, output, sourcePath);
					return false;
				}
			} catch (IOException e) {
				reportInternalError(Messages.SarlBatchCompiler_11, e);
			}
		}
		return true;
	}

	private static boolean isContainedIn(File child, File possibleParent) {
		var parent = child;
		while (parent != null) {
			if (parent.equals(possibleParent)) {
				return true;
			}
			parent = parent.getParentFile();
		}
		return false;
	}

	private static LinkedList<String> splitFile(File file, IProgressMonitor progress) {
		assert progress != null;
		final var elements = new LinkedList<String>();
		File current = file;
		do {
			if (progress.isCanceled()) {
				return null;
			}
			elements.addFirst(current.getName());
			current = current.getParentFile();
		} while (current != null);
		return elements;
	}

	private File determineCommonRoot(Iterable<File> files, IProgressMonitor progress) {
		assert progress != null;

		if (this.baseUri != null) {
			if (this.baseUri.isFile()) {
				if (getLogger().isLoggable(Level.FINEST)) {
					getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_32, this.baseUri));
				}
				return new File(this.baseUri.toFileString());
			}
			if (getLogger().isLoggable(Level.FINEST)) {
				getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_33, this.baseUri));
			}
		}

		LinkedList<String> longuestPrefix = null;

		for (final var file : files) {
			if (progress.isCanceled()) {
				return null;
			}
			if (file != null) {
				final var components = splitFile(file, progress);
				if (longuestPrefix == null) {
					longuestPrefix = components;
				} else {
					int i = 0;
					while (i < longuestPrefix.size() && i < components.size()
							&& Strings.equal(longuestPrefix.get(i), components.get(i))) {
						if (progress.isCanceled()) {
							return null;
						}
						++i;
					}
					while (i < longuestPrefix.size()) {
						if (progress.isCanceled()) {
							return null;
						}
						longuestPrefix.removeLast();
					}
					if (longuestPrefix.isEmpty()) {
						return null;
					}
				}
			}
		}

		if (longuestPrefix == null || progress.isCanceled()) {
			return null;
		}

		File prefix = null;
		for (final String component : longuestPrefix) {
			if (progress.isCanceled()) {
				return null;
			}
			if (prefix == null) {
				prefix = new File(component);
			} else {
				prefix = new File(prefix, component);
			}
		}

		return prefix;
	}

	private boolean configureWorkspace(ResourceSet resourceSet, IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_57);
		final var sourceFolders = getSourcePaths();
		final var javaOutputFile = getOutputPath();
		final var classOutputFile = getClassOutputPath();
		if (sourceFolders == null || sourceFolders.isEmpty() || javaOutputFile == null
				|| classOutputFile == null || progress.isCanceled()) {
			if (sourceFolders == null || sourceFolders.isEmpty()) {
				reportInternalError(Messages.SarlBatchCompiler_60);
			}
			if (javaOutputFile == null) {
				reportInternalError(Messages.SarlBatchCompiler_61);
			}
			if (classOutputFile == null) {
				reportInternalError(Messages.SarlBatchCompiler_62);
			}
			return false;
		}

		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_31, this.baseUri));
		}

		final var commonRoot = determineCommonRoot(
				Iterables.concat(sourceFolders, Arrays.asList(javaOutputFile, classOutputFile)),
				progress);
		if (progress.isCanceled()) {
			return false;
		}

		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_34, commonRoot));
		}
		if (commonRoot == null) {
			reportInternalError(Messages.SarlBatchCompiler_12);
			for (final var sourceFile : sourceFolders) {
				reportInternalError(Messages.SarlBatchCompiler_13, sourceFile);
			}
			reportInternalError(Messages.SarlBatchCompiler_14, javaOutputFile);
			return false;
		}
		this.projectConfig = new FileProjectConfig(commonRoot, commonRoot.getName());
		if (progress.isCanceled()) {
			return false;
		}

		final var commonURI = commonRoot.toURI();
		final var relativizedTarget = commonURI.relativize(javaOutputFile.toURI());
		if (progress.isCanceled()) {
			return false;
		}
		if (relativizedTarget.isAbsolute()) {
			reportInternalError(Messages.SarlBatchCompiler_15, javaOutputFile, commonRoot);
			return false;
		}
		final var slash = CharMatcher.is('/');
		final var relativeTargetFolder = slash.trimTrailingFrom(relativizedTarget.getPath());
		final var allOutputConfigurations = this.outputConfigurationProvider.getOutputConfigurations();
		if (progress.isCanceled()) {
			return false;
		}
		this.outputConfigurations = new TreeMap<>();
		for (final var configuration : allOutputConfigurations) {
			if (progress.isCanceled()) {
				return false;
			}
			this.outputConfigurations.put(configuration.getName(), configuration);
			if (Strings.equal(configuration.getName(), IFileSystemAccess.DEFAULT_OUTPUT)) {
				configuration.setOutputDirectory(new File(commonRoot, relativeTargetFolder).getAbsolutePath());
			} else {
				var outFile = new File(configuration.getOutputDirectory());
				outFile = new File(commonRoot, outFile.getPath());
				configuration.setOutputDirectory(outFile.getAbsolutePath());
			}
		}
		if (progress.isCanceled()) {
			return false;
		}
		for (final var source : sourceFolders) {
			if (progress.isCanceled()) {
				return false;
			}
			final var relSource = commonURI.relativize(source.toURI());
			if (relSource.isAbsolute()) {
				reportInternalError(Messages.SarlBatchCompiler_16, source, commonRoot);
				return false;
			}
			this.projectConfig.addSourceFolder(slash.trimTrailingFrom(relSource.getPath()));
		}
		if (progress.isCanceled()) {
			return false;
		}
		final var outputConfigurations = new HashMap<String, Set<OutputConfiguration>>();
		outputConfigurations.put(this.languageName, allOutputConfigurations);
		ProjectConfigAdapter.install(resourceSet, this.projectConfig);
		resourceSet.eAdapters().add(new OutputConfigurationAdapter(outputConfigurations));
		if (progress.isCanceled()) {
			return false;
		}
		return true;
	}

	/**
	 * Installs the JvmTypeProvider optionally including index access into the {@link ResourceSet}. The lookup classpath
	 * is enhanced with the given tmp directory.
	 *
	 * @param resourceSet the resource set that will be compiled.
	 * @param temporaryClassDirectory the directory where the class files of the stubs are generated.
	 * @param skipIndexLookup indicates if the index should be used for looking up types.
	 * @param cancelIndicator monitor for cancelling the compilation.
	 */
	@SuppressWarnings("unused")
	private void installJvmTypeProvider(ResourceSet resourceSet, File temporaryClassDirectory, boolean skipIndexLookup,
			IProgressMonitor progress) {
		assert progress != null;
		progress.subTask(Messages.SarlBatchCompiler_58);
		final Iterable<File> classpath;
		final Iterable<File> modulepath;
		if (temporaryClassDirectory != null) {
			if (isModularProject()) {
				classpath = getClassPath();
				modulepath = Iterables.concat(
						Collections.singletonList(temporaryClassDirectory),
						getModulePath(), getSourcePaths());
			} else {
				classpath = Iterables.concat(
						Collections.singletonList(temporaryClassDirectory),
						getClassPath(), getSourcePaths());
				modulepath = getModulePath();
			}
		} else {
			if (isModularProject()) {
				classpath = getClassPath();
				modulepath = Iterables.concat(
						getModulePath(), getSourcePaths());
			} else {
				classpath = Iterables.concat(
						getClassPath(), getSourcePaths());
				modulepath = getModulePath();
			}
		}
		if (getLogger().isLoggable(Level.FINEST)) {
			getLogger().finest(MessageFormat.format(Messages.SarlBatchCompiler_17, classpath));
		}
		if (progress.isCanceled()) {
			return;
		}
		final ClassLoader parentClassLoader;
		if (isUseCurrentClassLoaderAsParent()) {
			parentClassLoader = getClass().getClassLoader();
		} else {
			parentClassLoader = getCurrentClassLoader();
		}
		if (progress.isCanceled()) {
			return;
		}
		this.jvmTypesClassLoader = createClassLoader(classpath, modulepath, parentClassLoader);
		if (progress.isCanceled()) {
			return;
		}
		new ClasspathTypeProvider(this.jvmTypesClassLoader, resourceSet, skipIndexLookup ? null : this.indexedJvmTypeAccess, null);
		if (progress.isCanceled()) {
			return;
		}
		((XtextResourceSet) resourceSet).setClasspathURIContext(this.jvmTypesClassLoader);
		if (progress.isCanceled()) {
			return;
		}

		// for annotation processing we need to have the compiler's classpath as a parent.
		progress.subTask(Messages.SarlBatchCompiler_59);
		this.annotationProcessingClassLoader = createClassLoader(classpath, modulepath, getCurrentClassLoader());
		if (progress.isCanceled()) {
			return;
		}
		resourceSet.eAdapters().add(new ProcessorInstanceForJvmTypeProvider.ProcessorClassloaderAdapter(this.annotationProcessingClassLoader));
	}

	private static Iterable<URL> toURL(Iterable<File> files) {
		return Iterables.transform(files, from -> {
			try {
				final URL url = from.toURI().toURL();
				assert url != null;
				return url;
			} catch (Exception e) {
				throw new RuntimeException(e);
			}
		});
	}

	/** Create the project class loader.
	 *
	 * @param classPath the project class path.
	 * @param modulePath the project class path.
	 * @param parentClassLoader the parent class loader.
	 * @return the class loader for the project.
	 */
	@SuppressWarnings({ "static-method", "resource" })
	protected ClassLoader createClassLoader(Iterable<File> classPath, Iterable<File> modulePath, ClassLoader parentClassLoader) {
		return new URLClassLoader(Iterables.toArray(
				toURL(Iterables.concat(classPath, modulePath)),
				URL.class), parentClassLoader);
	}

	/** Null-safe destruction of the given class loaders.
	 *
	 * @param classLoader the class loader to destroy.
	 */
	protected void destroyClassLoader(ClassLoader classLoader) {
		if (classLoader instanceof Closeable ccl) {
			try {
				ccl.close();
			} catch (Exception e) {
				reportInternalWarning(Messages.SarlBatchCompiler_18, e);
			}
		}
	}

	/** Change the severity level of a warning.
	 *
	 * @param warningId the identifier of the warning. If {@code null} or empty, this function does nothing.
	 * @param severity the new severity. If {@code null} this function does nothing.
	 * @since 0.5
	 */
	public void setWarningSeverity(String warningId, Severity severity) {
		if (!Strings.isEmpty(warningId) && severity != null) {
			this.issueSeverityProvider.setSeverity(warningId, severity);
		}
	}

	/** Change the severity level of for all the warnings.
	 *
	 * @param severity the new severity. If {@code null} this function does nothing.
	 * @since 0.5
	 */
	public void setAllWarningSeverities(Severity severity) {
		if (severity != null) {
			this.issueSeverityProvider.setAllSeverities(severity);
		}
	}

	private static class LoggerRepositoryWrapper implements LoggerRepository {

		private final LoggerRepository original;

		private final Map<String, org.apache.log4j.Logger> wrapped = new HashMap<>();

		/** Construct a wrapper to the given repository.
		 *
		 * @param original the repository to wrap out.
		 */
		LoggerRepositoryWrapper(LoggerRepository original) {
			this.original = original;
		}

		/** Register a wrapping of a logger.
		 *
		 * @param loggerName the logger name.
		 * @param logger the logger.
		 */
		void registerWrapper(String loggerName, org.apache.log4j.Logger logger) {
			this.wrapped.put(loggerName, logger);
		}

		@Override
		public void addHierarchyEventListener(HierarchyEventListener listener) {
			this.original.addHierarchyEventListener(listener);
		}

		@Override
		public boolean isDisabled(int level) {
			return this.original.isDisabled(level);
		}

		@Override
		public void setThreshold(org.apache.log4j.Level level) {
			this.original.setThreshold(level);
		}

		@Override
		public void setThreshold(String val) {
			this.original.setThreshold(val);
		}

		@Override
		public void emitNoAppenderWarning(Category cat) {
			this.original.emitNoAppenderWarning(cat);
		}

		@Override
		public org.apache.log4j.Level getThreshold() {
			return this.original.getThreshold();
		}

		@Override
		public org.apache.log4j.Logger getLogger(String name) {
			final var wrapped = this.wrapped.get(name);
			if (wrapped != null) {
				return wrapped;
			}
			return this.original.getLogger(name);
		}

		@Override
		public org.apache.log4j.Logger getLogger(String name, LoggerFactory factory) {
			final var wrapped = this.wrapped.get(name);
			if (wrapped != null) {
				return wrapped;
			}
			return this.original.getLogger(name, factory);
		}

		@Override
		public org.apache.log4j.Logger getRootLogger() {
			return this.original.getRootLogger();
		}

		@Override
		public org.apache.log4j.Logger exists(String name) {
			return null;
		}

		@Override
		public void shutdown() {
			this.original.shutdown();
		}

		@Override
		public Enumeration<?> getCurrentLoggers() {
			return this.getCurrentLoggers();
		}

		@Override
		public Enumeration<?> getCurrentCategories() {
			return this.original.getCurrentCategories();
		}

		@Override
		public void fireAddAppenderEvent(Category logger, Appender appender) {
			this.original.fireAddAppenderEvent(logger, appender);
		}

		@Override
		public void resetConfiguration() {
			this.original.resetConfiguration();
		}

	}

}
