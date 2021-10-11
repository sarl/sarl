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

package io.sarl.maven.docs;

import java.io.File;
import java.io.IOError;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Properties;
import java.util.Set;
import java.util.TreeMap;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.base.Throwables;
import com.google.common.collect.Lists;
import com.google.common.io.Files;
import com.google.inject.Injector;
import com.google.inject.Key;
import com.google.inject.name.Names;
import org.apache.maven.artifact.Artifact;
import org.apache.maven.artifact.DependencyResolutionRequiredException;
import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.AbstractMojo;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.descriptor.PluginDescriptor;
import org.apache.maven.plugins.annotations.Component;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;
import org.apache.maven.shared.utils.io.DirectoryScanner;
import org.apache.maven.toolchain.Toolchain;
import org.apache.maven.toolchain.ToolchainManager;
import org.apache.maven.toolchain.ToolchainPrivate;
import org.apache.maven.toolchain.java.JavaToolchain;
import org.arakhne.afc.vmutil.FileSystem;
import org.codehaus.plexus.util.xml.Xpp3Dom;
import org.eclipse.xtext.util.JavaVersion;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.util.ReflectExtensions;

import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.maven.docs.markdown.MarkdownParser;
import io.sarl.maven.docs.parser.AbstractMarkerLanguageParser;
import io.sarl.maven.docs.parser.SarlDocumentationParser;
import io.sarl.maven.docs.parser.SarlDocumentationParser.ParsingException;
import io.sarl.maven.docs.testing.DocumentationLogger;
import io.sarl.maven.docs.testing.DocumentationSetup;
import io.sarl.maven.docs.testing.ScriptExecutor;

/** Abstract Maven MOJO for the documentation of the SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public abstract class AbstractDocumentationMojo extends AbstractMojo {

	/**
	 * Location of the temp directory.
	 */
	@Parameter(defaultValue = "${basedir}/target/documentation-temp", required = true)
	protected File tempDirectory;

	/**
	 * Location of the generated folder of the test sources.
	 */
	@Parameter(defaultValue = "${basedir}/target/generated-documentation-sources", required = true)
	protected String testSourceDirectory;

	/**
	 * Location of the source directories.
	 */
	@Parameter
	protected List<String> sourceDirectories;

	/** Default file encoding.
	 */
	@Parameter(defaultValue = "${project.build.sourceEncoding}", required = true)
	protected String encoding;

	/**
	 * Indicates if the source directories provided by Maven
	 * must be ignored if {@link #sourceDirectories} are manually provided.
	 */
	@Parameter(defaultValue = "true", required = true)
	protected boolean overrideSourceDirectories;

	/**
	 * The project itself. This parameter is set by maven.
	 */
	@Parameter(required = true, defaultValue = "${project}", readonly = true)
	protected MavenProject project;

	/**
	 * The base directory.
	 */
	@Parameter(required = true, defaultValue = "${basedir}", readonly = true)
	protected File baseDirectory;

	/**
	 * File extensions, including the dot character. Default is the file extension of the Markdown format.
	 */
	@Parameter(required = false)
	protected List<String> fileExtensions;

	/** File extension for the target language.
	 */
	protected String targetLanguageFileExtension;

	/**
	 * The current Maven session.
	 */
	@Parameter(defaultValue = "${session}", required = true, readonly = true)
	protected MavenSession session;

	/**
	 * Indicates if the Github extension should be enabled.
	 */
	@Parameter(defaultValue = "false", required = false)
	protected boolean githubExtension;

	/**
	 * Indicates if the line continuation syntax is enabled or not.
	 */
	@Parameter(defaultValue = "true", required = false)
	protected boolean isLineContinuationEnable;

	/**
	 * Java version number to support.
	 */
	@Parameter(required = false)
	protected String source;

	/** Injector.
	 */
	protected Injector injector;

	/** Inferred source directories.
	 */
	protected List<String> inferredSourceDirectories;

	/**
	 * The default values for the properties if they are not provided by any other source.
	 */
	@Parameter(required = false)
	private Properties propertyDefaultValues;

	@Component
	private ToolchainManager toolchainManager;

	private ReflectExtensions reflect;

	private static boolean isFileExtension(File filename, String[] extensions) {
		final String extension = FileSystem.extension(filename);
		for (final String ext : extensions) {
			if (Strings.equal(ext, extension)) {
				return true;
			}
		}
		return false;
	}

	/** Replies the message to display for skipping the execution of this mojo.
	 * If this function does not reply a message, the execution is not skipped.
	 * If this function replies a message, the execution is skipped.
	 *
	 * @return A message for skipping the execution, or {@code null} for executing the mojo.
	 */
	protected abstract String getSkippingMessage();

	@Override
	@SuppressWarnings("checkstyle:npathcomplexity")
	public final void execute() throws MojoExecutionException  {
		final String skipMessage = getSkippingMessage();
		if (!Strings.isEmpty(skipMessage)) {
			getLog().info(skipMessage);
			return;
		}
		if (this.fileExtensions == null || this.fileExtensions.isEmpty()) {
			this.fileExtensions = Arrays.asList(MarkdownParser.MARKDOWN_FILE_EXTENSIONS);
		}

		if (this.sourceDirectories == null) {
			this.sourceDirectories = Collections.emptyList();
		}
		if (!this.sourceDirectories.isEmpty() && this.overrideSourceDirectories) {
			this.inferredSourceDirectories = Lists.newArrayList(this.sourceDirectories);
		} else {
			this.inferredSourceDirectories = Lists.newArrayList(this.project.getCompileSourceRoots());
			this.inferredSourceDirectories.addAll(this.sourceDirectories);
			this.inferredSourceDirectories.add(Constants.DEFAULT_SOURCE_DIRECTORY);
		}

		getLog().info(Messages.AbstractDocumentationMojo_0);

		// Fix the logger configuration
		final Logger docLogger = DocumentationLogger.getLogger();
		final Handler[] handlers = docLogger.getHandlers();
		for (final Handler handler : handlers) {
			docLogger.removeHandler(handler);
		}
		docLogger.setUseParentHandlers(false);
		docLogger.setLevel(Level.ALL);
		docLogger.addHandler(new MavenJulHandler(getLog()));

		this.injector = DocumentationSetup.doSetup();
		assert this.injector != null;

		if (this.reflect == null) {
			this.reflect = this.injector.getInstance(ReflectExtensions.class);
		}

		this.targetLanguageFileExtension = this.injector.getInstance(Key.get(String.class, Names.named(
				org.eclipse.xtext.Constants.FILE_EXTENSIONS)));

		final String errorMessage = internalExecute();
		if (!Strings.isEmpty(errorMessage)) {
			throw new MojoExecutionException(errorMessage);
		}
	}

	/** Internal run.
	 *
	 * @return the error message
	 */
	protected String internalExecute() {
		getLog().info(Messages.AbstractDocumentationMojo_1);
		final Map<File, File> files = getFiles();
		getLog().info(MessageFormat.format(Messages.AbstractDocumentationMojo_2, files.size()));
		return internalExecute(files);
	}

	/** Internal run.
	 *
	 * @param files the map from source file to the source folder.
	 * @return the error message
	 * @throws UnsupportedOperationException a runtime exception.
	 */
	@SuppressWarnings("static-method")
	protected String internalExecute(Map<File, File> files) {
		throw new UnsupportedOperationException();
	}

	/** Execute the mojo on the given set of files.
	 *
	 * @param files the files
	 * @param outputFolder the output directory.
	 * @return the error message
	 */
	protected String internalExecute(Map<File, File> files, File outputFolder) {
		String firstErrorMessage = null;

		for (final Entry<File, File> entry : files.entrySet()) {
			final File inputFile = entry.getKey();
			try {
				final AbstractMarkerLanguageParser parser = createLanguageParser(inputFile);
				final File sourceFolder = entry.getValue();
				final File relativePath = FileSystem.makeRelative(inputFile, sourceFolder);
				internalExecute(sourceFolder, inputFile, relativePath, outputFolder, parser);
			} catch (Throwable exception) {
				final String errorMessage = formatErrorMessage(inputFile, exception);
				getLog().error(errorMessage);
				if (Strings.isEmpty(firstErrorMessage)) {
					firstErrorMessage = errorMessage;
				}
				getLog().debug(exception);
			}
		}
		return firstErrorMessage;
	}

	/** Execute the mojo on the given set of files.
	 *
	 * @param sourceFolder the source folder.
	 * @param inputFile the input file.
	 * @param relativeInputFile the name of the input file relatively to the source folder.
	 * @param outputFolder the output folder.
	 * @param parser the parser to be used for reading the input file.
	 * @throws IOException if there is some issue with IO.
	 * @throws UnsupportedOperationException a runtime exception.
	 */
	@SuppressWarnings("static-method")
	protected void internalExecute(File sourceFolder, File inputFile, File relativeInputFile, File outputFolder,
			AbstractMarkerLanguageParser parser)
			throws IOException {
		throw new UnsupportedOperationException();
	}

	/** Format the error message.
	 *
	 * @param inputFile the input file.
	 * @param exception the error.
	 * @return the error message.
	 */
	@SuppressWarnings("checkstyle:all")
	protected String formatErrorMessage(File inputFile, Throwable exception) {
		File filename;
		int lineno = 0;
		final boolean addExceptionName;
		if (exception instanceof ParsingException) {
			addExceptionName = false;
			final ParsingException pexception = (ParsingException) exception;
			final File file = pexception.getFile();
			if (file != null) {
				filename = file;
			} else {
				filename = inputFile;
			}
			lineno = pexception.getLineno();
		} else {
			addExceptionName = true;
			filename = inputFile;
		}
		for (final String sourceDir : this.session.getCurrentProject().getCompileSourceRoots()) {
			final File root = new File(sourceDir);
			if (isParentFile(filename, root)) {
				try {
					filename = FileSystem.makeRelative(filename, root);
				} catch (IOException exception1) {
					//
				}
				break;
			}
		}
		final StringBuilder msg = new StringBuilder();
		msg.append(filename.toString());
		if (lineno > 0) {
			msg.append(":").append(lineno); //$NON-NLS-1$
		}
		msg.append(": "); //$NON-NLS-1$
		final Throwable rootEx = Throwables.getRootCause(exception);
		if (rootEx != null && (addExceptionName || rootEx != exception)) {
			msg.append(rootEx.getClass().getName());
			msg.append(" - "); //$NON-NLS-1$
		}
		msg.append(rootEx.getLocalizedMessage());
		if (rootEx != null) {
			try (StringWriter swriter = new StringWriter()) {
				try (PrintWriter writer = new PrintWriter(swriter)) {
					rootEx.printStackTrace(writer);
				}
				msg.append("\n"); // $NON-NLS-1$
				msg.append(swriter.toString());
			} catch (IOException exception2) {
				throw new Error(exception2);
			}
		}
		return msg.toString();
	}

	private static boolean isParentFile(File file, File root) {
		if (file.isAbsolute() && root.isAbsolute()) {
			try {
				final String[] components1 = FileSystem.split(file.getCanonicalFile());
				final String[] components2 = FileSystem.split(root.getCanonicalFile());
				for (int i = 0; i < components2.length; ++i) {
					if (i >= components1.length || !Strings.equal(components2[i], components1[i])) {
						return false;
					}
				}
				return true;
			} catch (IOException exception) {
				//
			}
		}
		return false;
	}

	/** Create a parser for the given file.
	 *
	 * @param inputFile the file to be parsed.
	 * @return the parser.
	 * @throws MojoExecutionException if the parser cannot be created.
	 * @throws IOException if a classpath entry cannot be found.
	 */
	@SuppressWarnings({ "checkstyle:npathcomplexity" })
	protected AbstractMarkerLanguageParser createLanguageParser(File inputFile) throws MojoExecutionException, IOException {
		final JavaVersion javaVersion = SarlBatchCompilerUtils.parserJavaVersion(this.source);

		final AbstractMarkerLanguageParser parser;
		if (isFileExtension(inputFile, MarkdownParser.MARKDOWN_FILE_EXTENSIONS)) {
			parser = this.injector.getInstance(MarkdownParser.class);
		} else {
			throw new MojoExecutionException(MessageFormat.format(Messages.AbstractDocumentationMojo_3, inputFile));
		}
		parser.setGithubExtensionEnable(this.githubExtension);

		final SarlDocumentationParser internalParser = parser.getDocumentParser();

		if (this.isLineContinuationEnable) {
			internalParser.setLineContinuation(SarlDocumentationParser.DEFAULT_LINE_CONTINUATION);
		} else {
			internalParser.addLowPropertyProvider(createProjectProperties());
		}

		final ScriptExecutor scriptExecutor = internalParser.getScriptExecutor();
		final StringBuilder cp = new StringBuilder();
		final List<File> fullCp = getClassPath();
		for (final File cpElement : fullCp) {
			if (cp.length() > 0) {
				cp.append(File.pathSeparator);
			}
			cp.append(cpElement.getAbsolutePath());
		}
		scriptExecutor.setClassPath(cp.toString());
		final StringBuilder mp = new StringBuilder();
		final List<File> fullMp = getModulePath();
		for (final File mpElement : fullMp) {
			if (mp.length() > 0) {
				mp.append(File.pathSeparator);
			}
			mp.append(mpElement.getAbsolutePath());
		}
		scriptExecutor.setModulePath(mp.toString());
		scriptExecutor.setClassLoaderBuilder(it -> getProjectClassLoader(it, fullCp, fullCp.size()));
		scriptExecutor.setJavaSourceVersion(javaVersion.getQualifier());
		scriptExecutor.setTempFolder(this.tempDirectory.getAbsoluteFile());

		internalParser.addLowPropertyProvider(createProjectProperties());
		internalParser.addLowPropertyProvider(this.session.getCurrentProject().getProperties());
		internalParser.addLowPropertyProvider(this.session.getUserProperties());
		internalParser.addLowPropertyProvider(this.session.getSystemProperties());
		internalParser.addLowPropertyProvider(createGeneratorProperties());
		final Properties defaultValues = createDefaultValueProperties();
		if (defaultValues != null) {
			internalParser.addLowPropertyProvider(defaultValues);
		}
		return parser;
	}

	private Properties createDefaultValueProperties() {
		return this.propertyDefaultValues;
	}

	private Properties createProjectProperties() {
		final Properties props = new Properties();
		final MavenProject prj = this.session.getCurrentProject();
		props.put("project.groupId", Strings.emptyIfNull(prj.getGroupId())); //$NON-NLS-1$
		props.put("project.artifactId", Strings.emptyIfNull(prj.getArtifactId())); //$NON-NLS-1$
		props.put("project.basedir", prj.getBasedir() != null ? prj.getBasedir().getAbsolutePath() : ""); //$NON-NLS-1$ //$NON-NLS-2$
		props.put("project.description", Strings.emptyIfNull(prj.getDescription())); //$NON-NLS-1$
		props.put("project.id", Strings.emptyIfNull(prj.getId())); //$NON-NLS-1$
		props.put("project.inceptionYear", Strings.emptyIfNull(prj.getInceptionYear())); //$NON-NLS-1$
		props.put("project.name", Strings.emptyIfNull(prj.getName())); //$NON-NLS-1$
		props.put("project.version", Strings.emptyIfNull(prj.getVersion())); //$NON-NLS-1$
		props.put("project.url", Strings.emptyIfNull(prj.getUrl())); //$NON-NLS-1$
		props.put("project.encoding", Strings.emptyIfNull(this.encoding)); //$NON-NLS-1$
		return props;
	}

	private Properties createGeneratorProperties() {
		final Properties props = new Properties();
		final PluginDescriptor descriptor = (PluginDescriptor) getPluginContext().get("pluginDescriptor"); //$NON-NLS-1$
		props.put("generator.name", Strings.emptyIfNull(descriptor.getArtifactId())); //$NON-NLS-1$
		props.put("generator.version", Strings.emptyIfNull(descriptor.getVersion())); //$NON-NLS-1$
		return props;
	}

	/** Replies the source files.
	 *
	 * @return the map from the source files to the corresponding source folders.
	 */
	protected Map<File, File> getFiles() {
		final Map<File, File> files = new TreeMap<>();
		for (final String rootName : this.inferredSourceDirectories) {
			File root = FileSystem.convertStringToFile(rootName);
			if (!root.isAbsolute()) {
				root = FileSystem.makeAbsolute(root, this.baseDirectory);
			}
			getLog().debug(MessageFormat.format(Messages.AbstractDocumentationMojo_4, root.getName()));
			for (final File file : Files.fileTraverser().breadthFirst(root)) {
				if (file.exists() && file.isFile() && !file.isHidden() && file.canRead() && hasExtension(file)) {
					files.put(file, root);
				}
			}
		}
		return files;
	}

	private boolean hasExtension(File file) {
		final String extension = FileSystem.extension(file);
		return this.fileExtensions.contains(extension);
	}

	/** Convert a file to a package name.
	 *
	 * @param rootPackage an additional root package.
	 * @param packageName the file.
	 * @return the package name.
	 */
	protected static String toPackageName(String rootPackage,  File packageName) {
		final StringBuilder name = new StringBuilder();
		File tmp = packageName;
		while (tmp != null) {
			final String elementName = tmp.getName();
			if (!Strings.equal(FileSystem.CURRENT_DIRECTORY, elementName)
					&& !Strings.equal(FileSystem.PARENT_DIRECTORY, elementName)) {
				if (name.length() > 0) {
					name.insert(0, "."); //$NON-NLS-1$
				}
				name.insert(0, elementName);
			}
			tmp = tmp.getParentFile();
		}
		if (!Strings.isEmpty(rootPackage)) {
			if (name.length() > 0) {
				name.insert(0, "."); //$NON-NLS-1$
			}
			name.insert(0, rootPackage);
		}
		return name.toString();
	}

	/** Convert a a package name for therelative file.
	 *
	 * @param packageName the name.
	 * @return the file.
	 */
	protected static File toPackageFolder(String packageName) {
		File file = null;
		for (final String element : packageName.split("[.]")) { //$NON-NLS-1$
			if (file == null) {
				file = new File(element);
			} else {
				file = new File(file, element);
			}
		}
		return file;
	}

	/** Replies the current classpath.
	 *
	 * @return the current classpath.
	 * @throws IOException on failure.
	 */
	protected List<File> getClassPath() throws IOException {
		final Set<String> classPath = new LinkedHashSet<>();
		final MavenProject curProj = this.session.getCurrentProject();
		classPath.add(curProj.getBuild().getSourceDirectory());
		try {
			classPath.addAll(curProj.getCompileClasspathElements());
		} catch (DependencyResolutionRequiredException e) {
			throw new IOException(e.getLocalizedMessage(), e);
		}
		for (final Artifact dep : curProj.getArtifacts()) {
			classPath.add(dep.getFile().getAbsolutePath());
		}
		classPath.remove(curProj.getBuild().getOutputDirectory());
		final List<File> files = new ArrayList<>();
		for (final String filename : classPath) {
			final File file = new File(filename);
			if (file.exists()) {
				files.add(file);
			}
		}
		return files;
	}

	/** Replies the current module-path.
	 *
	 * @return the current module-path.
	 * @throws IOException on failure.
	 * @since 0.12
	 */
	protected List<File> getModulePath() throws IOException {
		final List<File> files = new ArrayList<>();
		return files;
	}

	/** Replies a class loader that is able to read the classes from the Maven class path.
	 *
	 * @param parent the parent class loader.
	 * @param classPath the additional class path.
	 * @param size the size of the class path.
	 * @return the class loader.
	 */
	private ClassLoader getProjectClassLoader(ClassLoader parent, Iterable<File> classPath, int size) {
		try {
			final URL[] urls = new URL[size];
			int i = 0;
			for (final File localFile : classPath) {
				final URL localUrl = FileSystem.convertFileToURL(localFile);
				if (localFile.isDirectory()) {
					final String name = localUrl.toExternalForm() + "/"; //$NON-NLS-1$
					urls[i] = new URL(name);
				} else {
					urls[i] = localUrl;
				}
				++i;
			}
			return new URLClassLoader(urls, getClass().getClassLoader());
		} catch (IOException exception) {
			throw new IOError(exception);
		}
	}

	/** Replies the boot classpath.
	 *
	 * @return the boot classpath.
	 * @throws IOException in case of error.
	 * @deprecated since 0.12, will be remove definitively when Jaa 8 is no more supported.
	 */
	@Deprecated
	protected String getBootClassPath() throws IOException {
		final Toolchain toolchain = this.toolchainManager.getToolchainFromBuildContext("jdk", this.session); //$NON-NLS-1$
		if (toolchain instanceof JavaToolchain && toolchain instanceof ToolchainPrivate) {
			final JavaToolchain javaToolChain = (JavaToolchain) toolchain;
			final ToolchainPrivate privateJavaToolChain = (ToolchainPrivate) toolchain;
			String[] includes = {"jre/lib/*", "jre/lib/ext/*", "jre/lib/endorsed/*"}; //$NON-NLS-1$ //$NON-NLS-2$ //$NON-NLS-3$
			String[] excludes = new String[0];
			final Xpp3Dom config = (Xpp3Dom) privateJavaToolChain.getModel().getConfiguration();
			if (config != null) {
				final Xpp3Dom bootClassPath = config.getChild("bootClassPath"); //$NON-NLS-1$
				if (bootClassPath != null) {
					final Xpp3Dom includeParent = bootClassPath.getChild("includes"); //$NON-NLS-1$
					if (includeParent != null) {
						includes = getValues(includeParent.getChildren("include")); //$NON-NLS-1$
					}
					final Xpp3Dom excludeParent = bootClassPath.getChild("excludes"); //$NON-NLS-1$
					if (excludeParent != null) {
						excludes = getValues(excludeParent.getChildren("exclude")); //$NON-NLS-1$
					}
				}
			}

			try {
				return scanBootclasspath(Objects.toString(this.reflect.invoke(javaToolChain, "getJavaHome")), includes, excludes); //$NON-NLS-1$
			} catch (Exception e) {
				throw new IOException(e.getLocalizedMessage(), e);
			}
		}
		return ""; //$NON-NLS-1$
	}

	private static String scanBootclasspath(String javaHome, String[] includes, String[] excludes) {
		final DirectoryScanner scanner = new DirectoryScanner();
		scanner.setBasedir(new File(javaHome));
		scanner.setIncludes(includes);
		scanner.setExcludes(excludes);
		scanner.scan();

		final StringBuilder bootClassPath = new StringBuilder();
		final String[] includedFiles = scanner.getIncludedFiles();
		for (int i = 0; i < includedFiles.length; i++) {
			if (i > 0) {
				bootClassPath.append(File.pathSeparator);
			}
			bootClassPath.append(new File(javaHome, includedFiles[i]).getAbsolutePath());
		}
		return bootClassPath.toString();
	}

	private static String[] getValues(Xpp3Dom[] children) {
		final String[] values = new String[children.length];
		for (int i = 0; i < values.length; i++) {
			values[i] = children[i].getValue();
		}
		return values;
	}

}
