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

package io.sarl.sarldoc.commands;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Logger;
import javax.inject.Provider;

import com.google.common.base.Strings;
import com.google.common.collect.Multimap;
import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.meta.application.CommandMetadata;
import org.arakhne.afc.vmutil.DynamicURLClassLoader;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.mwe.PathTraverser;
import org.eclipse.xtext.util.JavaVersion;

import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.ClassPathUtils;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.maven.bootiqueapp.BootiqueMain;
import io.sarl.maven.bootiqueapp.utils.SystemPath;
import io.sarl.sarldoc.Constants;
import io.sarl.sarldoc.configs.SarldocConfig;
import io.sarl.sarldoc.configs.Tag;

/**
 * Abstract command for launching sarldoc that is sharing all the code
 * between the real sarldoc command and its fake version.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public abstract class AbstractSarldocCommand extends CommandWithMetadata {

	/** Empty string.
	 */
	protected static final String EMPTY_STRING = ""; //$NON-NLS-1$

	private static final String HTTP_PROTOCOL_NAME = "http"; //$NON-NLS-1$

	private static final String HTTP_HOST_PROPERTY_NAME = "http.proxyHost"; //$NON-NLS-1$

	private static final String HTTP_HOST_VARIABLE_NAME = "http_proxy"; //$NON-NLS-1$

	private static final String HTTP_NONPROXYHOST_PROPERTY_NAME = "http.nonProxyHosts"; //$NON-NLS-1$

	private static final String HTTP_PORT_PROPERTY_NAME = "http.proxyPort"; //$NON-NLS-1$

	private static final String HTTPS_PROTOCOL_NAME = "https"; //$NON-NLS-1$

	private static final String HTTPS_HOST_PROPERTY_NAME = "https.proxyHost"; //$NON-NLS-1$

	private static final String HTTPS_HOST_VARIABLE_NAME = "https_proxy"; //$NON-NLS-1$

	private static final String HTTPS_NONPROXYHOST_PROPERTY_NAME = "https.nonProxyHosts"; //$NON-NLS-1$

	private static final String HTTPS_PORT_PROPERTY_NAME = "https.proxyPort"; //$NON-NLS-1$

	private static final String SIMPLE_URL_PATTERN = "%s://%s"; //$NON-NLS-1$

	private static final String PORT_URL_PATTERN = "%s://%s:%s"; //$NON-NLS-1$

	private static final String PACKAGE_SEPARATOR = "."; //$NON-NLS-1$

	private static final String JAVA_FILE_EXTENSION = "java"; //$NON-NLS-1$

	private static final String LOCALE_FLAG = "-locale"; //$NON-NLS-1$

	private static final String DOCENCODING_FLAG = "-docencoding"; //$NON-NLS-1$

	private static final String ENCODING_FLAG = "-encoding"; //$NON-NLS-1$

	private static final String SOURCE_FLAG = "-source"; //$NON-NLS-1$

	private static final String DOCTITLE_FLAG = "-doctitle"; //$NON-NLS-1$

	private static final String NODEPRECATED_FLAG = "-nodeprecated"; //$NON-NLS-1$

	private static final String NODEPRECATEDLIST_FLAG = "-nodeprecatedlist"; //$NON-NLS-1$

	private static final String NOSINCE_FLAG = "-nosince"; //$NON-NLS-1$

	private static final String AUTHOR_FLAG = "-author"; //$NON-NLS-1$

	private static final String VERSION_FLAG = "-version"; //$NON-NLS-1$

	private static final String PUBLIC_FLAG = "-public"; //$NON-NLS-1$

	private static final String PROTECTED_FLAG = "-protected"; //$NON-NLS-1$

	private static final String PACKAGE_FLAG = "-package"; //$NON-NLS-1$

	private static final String PRIVATE_FLAG = "-private"; //$NON-NLS-1$

	private static final String SOURCEPATH_FLAG = "-sourcepath"; //$NON-NLS-1$

	private static final String CLASSPATH_FLAG = "-classpath"; //$NON-NLS-1$

	private static final String MODULEPATH_FLAG = "-modulepath"; //$NON-NLS-1$

	private static final String SMALLD_FLAG = "-d"; //$NON-NLS-1$

	private static final String DOCLET_FLAG = "-doclet"; //$NON-NLS-1$

	private static final String DOCLETPATH_FLAG = "-docletpath"; //$NON-NLS-1$

	private static final String TAG_FLAG = "-tag"; //$NON-NLS-1$

	private static final String TOOLS_JAR_INTERNAL_CLASS = "com.sun.tools.doclets.internal.toolkit.AbstractDoclet"; //$NON-NLS-1$

	private final Provider<SarldocConfig> config;

	private final Provider<SarlcConfig> sarlcConfig;

	private final Provider<SARLClasspathProvider> defaultClasspath;

	private final Provider<PathDetector> pathDetector;

	private final Provider<Logger> logger;

	private final DynamicURLClassLoader sarldocClassLoader;

	/** Constructor.
	 *
	 * @param sarldocClassLoader the dynamic class loader that could be used by sarldoc.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param defaultClasspath the provider of default classpaths.
	 * @param pathDetector the detector of paths.
	 * @param name the name of the commande, or {@code null} if none.
	 */
	public AbstractSarldocCommand(DynamicURLClassLoader sarldocClassLoader, Provider<Logger> logger,
			Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<SARLClasspathProvider> defaultClasspath, Provider<PathDetector> pathDetector, String name) {
		super(addName(name, CommandMetadata
				.builder(AbstractSarldocCommand.class)
				.description(Messages.SarldocCommand_0)));
		this.sarldocClassLoader = sarldocClassLoader;
		this.logger = logger;
		this.config = config;
		this.sarlcConfig = sarlcConfig;
		this.defaultClasspath = defaultClasspath;
		this.pathDetector = pathDetector;
	}

	private static CommandMetadata.Builder addName(String name, CommandMetadata.Builder builder) {
		if (!Strings.isNullOrEmpty(name)) {
			return builder.name(name);
		}
		return builder;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final Logger logger = this.logger.get();
		final SarldocConfig dconfig = this.config.get();
		// Force proxy definition
		forceProxyDefinition(dconfig, logger);
		// Run sarlc
		CommandOutcome outcome = runSarlc(cli, logger);
		if (outcome.isSuccess()) {
			// Run sarldoc
			final SarlcConfig cconfig = this.sarlcConfig.get();
			final AtomicInteger errorCount = new AtomicInteger();
			final AtomicInteger warningCount = new AtomicInteger();
			outcome = runJavadoc(cli, dconfig, cconfig, logger, errorCount, warningCount);
			if (outcome == null || outcome.isSuccess()) {
				if (warningCount.get() > 0) {
					if (warningCount.get() > 1) {
						logger.info(MessageFormat.format(Messages.SarldocCommand_7, warningCount.get()));
					} else {
						logger.info(MessageFormat.format(Messages.SarldocCommand_6, warningCount.get()));
					}
				} else {
					logger.info(Messages.SarldocCommand_4);
				}
			}
		}
		return outcome;
	}

	/** Run the SARL compiler.
	 *
	 * @param cli the command line instructions.
	 * @param logger the logger to use for notifying the user.
	 * @return the state of the command.
	 */
	protected abstract CommandOutcome runSarlc(Cli cli, Logger logger);

	private static void addProxyFromProperty(Map<String, URI> activeProxies, String protocol, String hostVar, String portVar) {
		final String host = System.getProperty(hostVar, null);
		if (!Strings.isNullOrEmpty(host)) {
			final String port = System.getProperty(portVar, null);
			final URI uri;
			try {
				if (Strings.isNullOrEmpty(port)) {
					uri = new URI(String.format(SIMPLE_URL_PATTERN, protocol, host));
				} else {
					uri = new URI(String.format(PORT_URL_PATTERN, protocol, host, port));
				}
				activeProxies.putIfAbsent(protocol, uri);
			} catch (Throwable exception) {
				// Silent error
			}
		}
	}

	private static void addProxyFromEnvironment(Map<String, URI> activeProxies, String protocol, String var) {
        try {
        	final String host = System.getenv(var);
    		if (!Strings.isNullOrEmpty(host)) {
    			try {
    				final URI uri = new URI(host);
    				activeProxies.putIfAbsent(protocol, uri);
    			} catch (Throwable exception) {
    				// Silent error
    			}
    		}
        } catch (final SecurityException ex) {
            // Silent
        }
	}

	private static String ifNotEmpty(String value) {
		if (Strings.isNullOrEmpty(value)) {
			return null;
		}
		return value;
	}

	private static String ifNotZero(int value) {
		if (value == 0) {
			return null;
		}
		return ifNotEmpty(Integer.toString(value));
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private static void forceProxyDefinition(SarldocConfig config, Logger logger) {
		final Map<String, URI> activeProxies = new HashMap<>();

		// Read the proxy definitions from the OS environment, the VM properties, and the sarldoc configuration in that order
		addProxyFromEnvironment(activeProxies, HTTP_PROTOCOL_NAME, HTTP_HOST_VARIABLE_NAME);
		addProxyFromEnvironment(activeProxies, HTTPS_PROTOCOL_NAME, HTTPS_HOST_VARIABLE_NAME);

		addProxyFromProperty(activeProxies, HTTP_PROTOCOL_NAME, HTTP_HOST_PROPERTY_NAME, HTTP_PORT_PROPERTY_NAME);
		addProxyFromProperty(activeProxies, HTTPS_PROTOCOL_NAME, HTTPS_HOST_PROPERTY_NAME, HTTPS_PORT_PROPERTY_NAME);

		for (final String proxyDefinition : config.getProxy()) {
			try {
				final URI proxy = new URI(proxyDefinition);
				if (!Strings.isNullOrEmpty(proxy.getHost())) {
					activeProxies.putIfAbsent(proxy.getScheme(), proxy);
				}
			} catch (Throwable exception) {
				//
			}
		}

		// Extract HTTPS configuration
		URI uri = activeProxies.get(HTTPS_PROTOCOL_NAME);
		boolean hasHttps = false;
		String httpsHost = null;
		String httpsPort = null;
		String httpsNoProxy = config.getHttpsNoProxyHostsString();
		if (uri != null) {
			httpsHost = ifNotEmpty(uri.getHost());
			hasHttps = !Strings.isNullOrEmpty(httpsHost);
			if (hasHttps) {
				httpsNoProxy = System.getProperty(HTTPS_NONPROXYHOST_PROPERTY_NAME, httpsNoProxy);
				if (uri.getPort() != 0) {
					httpsPort = ifNotZero(uri.getPort());
				}
			}
		}

		// Extract HTTP configuration, and if not HTTPS was provided, assumes HTTPS is the same as HTTP.
		uri = activeProxies.get(HTTP_PROTOCOL_NAME);
		boolean hasHttp = false;
		String httpHost = null;
		String httpPort = null;
		String httpNoProxy = config.getHttpNoProxyHostsString();
		if (uri != null) {
			httpHost = ifNotEmpty(uri.getHost());
			hasHttp = !Strings.isNullOrEmpty(httpHost);
			if (hasHttp) {
				httpNoProxy = System.getProperty(HTTP_NONPROXYHOST_PROPERTY_NAME, httpNoProxy);
				if (uri.getPort() != 0) {
					httpPort = ifNotZero(uri.getPort());
				}
			}
			if (!hasHttps && hasHttp) {
				httpsHost = httpHost;
				httpsPort = httpPort;
				httpsNoProxy = httpNoProxy;
				hasHttps = true;
			}
		}

		// Overrive the proxy configurations
		if (hasHttp) {
			final String url;
			if (Strings.isNullOrEmpty(httpPort)) {
				url = String.format(SIMPLE_URL_PATTERN, HTTP_PROTOCOL_NAME, httpHost);
			} else {
				url = String.format(PORT_URL_PATTERN, HTTP_PROTOCOL_NAME, httpHost, httpPort);
			}
			logger.info(MessageFormat.format(Messages.SarldocCommand_8, HTTP_PROTOCOL_NAME, url));
			System.setProperty(HTTP_HOST_PROPERTY_NAME, httpHost);
			System.setProperty(HTTP_PORT_PROPERTY_NAME, httpPort);
			logger.fine(MessageFormat.format(Messages.SarldocCommand_9, HTTP_PROTOCOL_NAME, httpNoProxy));
			System.setProperty(HTTP_NONPROXYHOST_PROPERTY_NAME, httpNoProxy);
		}

		if (hasHttps) {
			final String url;
			if (Strings.isNullOrEmpty(httpsPort)) {
				url = String.format(SIMPLE_URL_PATTERN, HTTPS_PROTOCOL_NAME, httpsHost);
			} else {
				url = String.format(PORT_URL_PATTERN, HTTPS_PROTOCOL_NAME, httpsHost, httpsPort);
			}
			logger.info(MessageFormat.format(Messages.SarldocCommand_8, HTTPS_PROTOCOL_NAME, url));
			System.setProperty(HTTPS_HOST_PROPERTY_NAME, httpsHost);
			System.setProperty(HTTPS_PORT_PROPERTY_NAME, httpsPort);
			logger.fine(MessageFormat.format(Messages.SarldocCommand_9, HTTPS_PROTOCOL_NAME, httpsNoProxy));
			System.setProperty(HTTPS_NONPROXYHOST_PROPERTY_NAME, httpsNoProxy);
		}
	}

	@SuppressWarnings({"checkstyle:npathcomplexity", "checkstyle:cyclomaticcomplexity"})
	private CommandOutcome runJavadoc(Cli cli, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger errorCount, AtomicInteger warningCount) throws IllegalArgumentException {
		logger.info(Messages.SarldocCommand_2);
		final String javadocExecutable = docconfig.getJavadocExecutable();
		final List<String> cmd = new ArrayList<>();

		// Locale
		cmd.add(LOCALE_FLAG);
		cmd.add(docconfig.getLocale());

		// Encoding
		final String encoding = docconfig.getEncoding();
		cmd.add(DOCENCODING_FLAG);
		cmd.add(encoding);
		cmd.add(ENCODING_FLAG);
		cmd.add(encoding);

		// Javadoc Options
		for (final String option : docconfig.getJavadocOption()) {
			cmd.add(option);
		}

		// Java version
		final JavaVersion javaVersion = SarlBatchCompilerUtils.parserJavaVersion(cconfig.getCompiler().getJavaVersion());
		cmd.add(SOURCE_FLAG);
		cmd.add(javaVersion.getQualifier());

		// Documentation title
		final String title = docconfig.getTitle();
		if (!Strings.isNullOrEmpty(title)) {
			cmd.add(DOCTITLE_FLAG);
			cmd.add(title);
		}

		// Special tags
		if (!docconfig.getEnableDeprecatedTag()) {
			cmd.add(NODEPRECATED_FLAG);
			cmd.add(NODEPRECATEDLIST_FLAG);
		}

		if (!docconfig.getEnableSinceTag()) {
			cmd.add(NOSINCE_FLAG);
		}

		if (docconfig.getEnableVersionTag()) {
			cmd.add(VERSION_FLAG);
		}

		if (docconfig.getEnableAuthorTag()) {
			cmd.add(AUTHOR_FLAG);
		}

		// Visibility of the elements
		switch (docconfig.getVisibility()) {
		case PUBLIC:
			cmd.add(PUBLIC_FLAG);
			break;
		case PROTECTED:
			cmd.add(PROTECTED_FLAG);
			break;
		case PACKAGE:
			cmd.add(PACKAGE_FLAG);
			break;
		case PRIVATE:
			cmd.add(PRIVATE_FLAG);
			break;
		default:
			throw new IllegalStateException();
		}

		// Path detection
		final PathDetector paths = this.pathDetector.get();
		paths.setSarlOutputPath(cconfig.getOutputPath());
		paths.setClassOutputPath(cconfig.getClassOutputPath());
		paths.setTempDirectory(cconfig.getTempDirectory());
		try {
			paths.resolve(cli.standaloneArguments());
		} catch (IOException exception) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
		}

		// Source folder
		final SystemPath sourcePath = new SystemPath();
		for (final String path : cli.standaloneArguments()) {
			sourcePath.add(path);
		}
		if (paths.getSarlOutputPath() != null) {
			sourcePath.add(paths.getSarlOutputPath());
		}
		cmd.add(SOURCEPATH_FLAG);
		cmd.add(sourcePath.toString());

		// Class path
		final SARLClasspathProvider classpathProvider = this.defaultClasspath.get();
		final SystemPath fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cconfig, javaVersion, logger);
		final SystemPath fullModulePath = ClassPathUtils.buildModulePath(classpathProvider, cconfig, javaVersion, logger);

		File toolsjar = null;

		try {
			if (this.sarldocClassLoader.loadClass(TOOLS_JAR_INTERNAL_CLASS) == null) {
				throw new ClassNotFoundException();
			}
		} catch (ClassNotFoundException | NoClassDefFoundError error) {
			// The internal class of "tools.jar" cannot be loaded.
			// It is usually due to the fact that "tools.jar" is missed from the class path.
			// This error should occurs when running sarldoc with the jar file with all dependencies.
			// Indeed, "tools.jar" is marked as a system dependency (because it is part of the JDK),
			// it is not included into the jar file of sarldoc itself.
			try {
				toolsjar = SarldocConfig.findToolsJar();
				fullClassPath.add(toolsjar);
				this.sarldocClassLoader.addURL(FileSystem.convertFileToURL(toolsjar));
			} catch (FileNotFoundException exception) {
				return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception.getMessage());
			}
			// Try again
			try {
				if (this.sarldocClassLoader.loadClass(TOOLS_JAR_INTERNAL_CLASS) == null) {
					return CommandOutcome.failed(BootiqueMain.ERROR_CODE,
							io.sarl.sarldoc.configs.Messages.SarldocConfig_3);
				}
			} catch (ClassNotFoundException | NoClassDefFoundError error1) {
				return CommandOutcome.failed(BootiqueMain.ERROR_CODE,
						io.sarl.sarldoc.configs.Messages.SarldocConfig_3);
			}
		}

		cmd.add(CLASSPATH_FLAG);
		cmd.add(fullClassPath.toString());

		if (SarlBatchCompilerUtils.isModuleSupported(javaVersion)) {
			cmd.add(MODULEPATH_FLAG);
			cmd.add(fullModulePath.toString());
		}

		// Output folder
		cmd.add(SMALLD_FLAG);
		cmd.add(docconfig.getDocumentationOutputDirectory().getAbsolutePath());

		// Doclet
		cmd.add(DOCLET_FLAG);
		String docletName = docconfig.getDoclet();
		if (Strings.isNullOrEmpty(docletName)) {
			docletName = Constants.DEFAULT_DOCLET;
		}
		cmd.add(docletName);

		// Doclet path
		final SystemPath docletPath = new SystemPath();
		docletPath.addEntries(fullClassPath);
		docletPath.addEntries(docconfig.getDocletPath());
		if (!docletPath.isEmpty()) {
			cmd.add(DOCLETPATH_FLAG);
			cmd.add(docletPath.toString());
		}

		// Add custom tags
		for (final Tag tag : docconfig.getCustomTags()) {
			cmd.add(TAG_FLAG);
			cmd.add(tag.toString());
		}

		// Add source files
		final Collection<String> files = getSourceFiles(sourcePath, docconfig);
		for (final String sourceFile : files) {
			cmd.add(sourceFile);
		}

		// Execute the Javadoc
		return runJavadoc(this.sarldocClassLoader, javadocExecutable, cmd, docconfig, cconfig, logger, errorCount, warningCount);
	}

	/** Run the Javadoc binary based on the provided arguments.
	 *
	 * @param classLoader the class loader to be used for launching the javadoc tool.
	 * @param javadocExecutable the executable for javadoc.
	 * @param cmd is the full description of the javadoc command-line instruction.
	 * @param docconfig the configuration of the sarldoc tool.
	 * @param cconfig the configuration of the sarlc tool.
	 * @param logger the logger to be used for ontifiy the user.
	 * @param errorCount the number of errors found into the project.
	 * @param warningCount the number of warnings found into the project.
	 * @return the result of the execution.
	 */
	protected abstract CommandOutcome runJavadoc(DynamicURLClassLoader classLoader, String javadocExecutable,
			List<String> cmd, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger errorCount, AtomicInteger warningCount);

	private static boolean isExcludedPackage(File file, SarldocConfig config) {
		if (file == null) {
			return true;
		}
		final Set<String> excl = config.getExcludedPackages();
		if (excl.isEmpty()) {
			return false;
		}
		final StringBuilder name = new StringBuilder();
		File cfile = file.getParentFile();
		boolean first = true;
		while (cfile != null && !Objects.equals(cfile.getName(), FileSystem.CURRENT_DIRECTORY)
				&& !Objects.equals(cfile.getName(), FileSystem.PARENT_DIRECTORY)) {
			if (first) {
				first = false;
			} else {
				name.insert(0, PACKAGE_SEPARATOR);
			}
			name.insert(0, cfile.getName());
			cfile = cfile.getParentFile();
		}
		return !excl.contains(name.toString());
	}

	private static Collection<String> getSourceFiles(SystemPath sourcePaths, SarldocConfig config) {
		final Set<String> allFiles = new TreeSet<>();
		final PathTraverser pathTraverser = new PathTraverser();
		final Multimap<String, org.eclipse.emf.common.util.URI> pathes = pathTraverser.resolvePathes(
			sourcePaths.toFilenameList(),
			input -> Objects.equals(JAVA_FILE_EXTENSION, input.fileExtension()));
		for (final Entry<String, org.eclipse.emf.common.util.URI> entry : pathes.entries()) {
			final String filename = entry.getValue().toFileString();
			final File file = FileSystem.convertStringToFile(filename);
			final File root = FileSystem.convertStringToFile(entry.getKey());
			try {
				if (!isExcludedPackage(FileSystem.makeRelative(file, root), config)) {
					allFiles.add(filename);
				}
			} catch (Throwable exception) {
				// Silent exception
			}
		}
		return allFiles;
	}

}
