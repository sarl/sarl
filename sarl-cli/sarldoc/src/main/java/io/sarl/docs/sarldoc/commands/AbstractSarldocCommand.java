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

package io.sarl.docs.sarldoc.commands;

import java.io.File;
import java.io.IOException;
import java.net.URI;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.TreeSet;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.logging.Level;
import java.util.logging.Logger;

import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.mwe.PathTraverser;

import com.google.common.base.Strings;

import io.bootique.cli.Cli;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.meta.application.CommandMetadata;
import io.sarl.apputils.bootiqueapp.BootiqueMain;
import io.sarl.apputils.bootiqueapp.utils.SystemPath;
import io.sarl.docs.doclet2.Doclet;
import io.sarl.docs.doclet2.html.SarlHtmlDocletOptions;
import io.sarl.docs.sarldoc.configs.SarldocConfig;
import io.sarl.docs.sarldoc.tools.DocumentationPathDetector;
import io.sarl.lang.compiler.batch.SarlBatchCompilerUtils;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import jakarta.inject.Inject;
import jakarta.inject.Provider;

/**
 * Abstract command for launching sarldoc that is sharing all the code
 * between the real sarldoc command and its fake version.
 *
 * @author $Author: sgalland$
 * @version sarldoc 0.15.0 20250909-115750
 * @mavengroupid io.sarl.cli
 * @mavenartifactid sarldoc
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

	private static final String ENCODING_FLAG = "-encoding"; //$NON-NLS-1$

	private static final String SOURCE_FLAG = "-source"; //$NON-NLS-1$

	private static final String DOCTITLE_FLAG = "-doctitle"; //$NON-NLS-1$

	private static final String PUBLIC_FLAG = "-public"; //$NON-NLS-1$

	private static final String PROTECTED_FLAG = "-protected"; //$NON-NLS-1$

	private static final String PACKAGE_FLAG = "-package"; //$NON-NLS-1$

	private static final String PRIVATE_FLAG = "-private"; //$NON-NLS-1$

	private static final String TAG_FLAG = "-tag"; //$NON-NLS-1$

	private final Provider<SarldocConfig> config;

	private final Provider<SarlcConfig> sarlcConfig;

	private final Provider<DocumentationPathDetector> pathDetector;

	private final Provider<Logger> logger;

	/** Constructor.
	 *
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param pathDetector the detector of paths.
	 * @param name the name of the commande, or {@code null} if none.
	 */
	@SuppressWarnings("removal")
	@Inject
	public AbstractSarldocCommand(Provider<Logger> logger,
			Provider<SarldocConfig> config, Provider<SarlcConfig> sarlcConfig,
			Provider<DocumentationPathDetector> pathDetector, String name) {
		super(addName(name, CommandMetadata
				.builder(AbstractSarldocCommand.class)
				.description(Messages.SarldocCommand_0)));
		this.logger = logger;
		this.config = config;
		this.sarlcConfig = sarlcConfig;
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
		final var logger = this.logger.get();
		final var dconfig = this.config.get();
		// Force proxy definition
		forceProxyDefinition(dconfig, logger);
		// Run sarlc
		var outcome = runSarlc(cli, logger);
		if (outcome.isSuccess()) {
			// Run sarldoc
			final var cconfig = this.sarlcConfig.get();
			final var errorCount = new AtomicInteger();
			final var warningCount = new AtomicInteger();
			outcome = runJavadoc(cli, dconfig, cconfig, logger, errorCount, warningCount);
			if (outcome == null || outcome.isSuccess()) {
				if (warningCount.get() > 0) {
					if (warningCount.get() > 1) {
						logger.info(MessageFormat.format(Messages.SarldocCommand_7, Integer.valueOf(warningCount.get())));
					} else {
						logger.info(MessageFormat.format(Messages.SarldocCommand_6, Integer.valueOf(warningCount.get())));
					}
				} else {
					logger.info(Messages.SarldocCommand_4);
				}
			} else if (outcome.getException() != null) {
				logger.log(Level.SEVERE, outcome.getMessage(), outcome.getException());
			} else {
				logger.log(Level.SEVERE, outcome.getMessage());
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
		final var host = System.getProperty(hostVar, null);
		if (!Strings.isNullOrEmpty(host)) {
			final var port = System.getProperty(portVar, null);
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
        	final var host = System.getenv(var);
    		if (!Strings.isNullOrEmpty(host)) {
    			try {
    				final var uri = new URI(host);
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

	private static void forceProxyDefinition(SarldocConfig config, Logger logger) {
		final var activeProxies = new HashMap<String, URI>();

		// Read the proxy definitions from the OS environment, the VM properties, and the sarldoc configuration in that order
		addProxyFromEnvironment(activeProxies, HTTP_PROTOCOL_NAME, HTTP_HOST_VARIABLE_NAME);
		addProxyFromEnvironment(activeProxies, HTTPS_PROTOCOL_NAME, HTTPS_HOST_VARIABLE_NAME);

		addProxyFromProperty(activeProxies, HTTP_PROTOCOL_NAME, HTTP_HOST_PROPERTY_NAME, HTTP_PORT_PROPERTY_NAME);
		addProxyFromProperty(activeProxies, HTTPS_PROTOCOL_NAME, HTTPS_HOST_PROPERTY_NAME, HTTPS_PORT_PROPERTY_NAME);

		for (final var proxyDefinition : config.getProxy()) {
			try {
				final var proxy = new URI(proxyDefinition);
				if (!Strings.isNullOrEmpty(proxy.getHost())) {
					activeProxies.putIfAbsent(proxy.getScheme(), proxy);
				}
			} catch (Throwable exception) {
				//
			}
		}

		// Extract HTTPS configuration
		var uri = activeProxies.get(HTTPS_PROTOCOL_NAME);
		var hasHttps = false;
		String httpsHost = null;
		String httpsPort = null;
		var httpsNoProxy = config.getHttpsNoProxyHostsString();
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
		var hasHttp = false;
		String httpHost = null;
		String httpPort = null;
		var httpNoProxy = config.getHttpNoProxyHostsString();
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

	private static void addCmd(List<String> cmd, String option, String value) {
		if (!Strings.isNullOrEmpty(value)) {
			cmd.add(option);
			cmd.add(value);
		}
	}

	private CommandOutcome runJavadoc(Cli cli, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger errorCount, AtomicInteger warningCount) throws IllegalArgumentException {
		logger.info(Messages.SarldocCommand_2);
		final var cmd = new ArrayList<String>();

		// Locale
		addCmd(cmd, LOCALE_FLAG, docconfig.getLocale());

		// Encoding
		final var encoding = docconfig.getEncoding();
		addCmd(cmd, ENCODING_FLAG, encoding);

		// Java version
		final var javaVersion = SarlBatchCompilerUtils.parseJavaVersion(cconfig.getCompiler().getJavaVersion());
		addCmd(cmd, SOURCE_FLAG, javaVersion.getQualifier());

		// Documentation title
		final var title = docconfig.getTitle();
		addCmd(cmd, DOCTITLE_FLAG, title);
		addCmd(cmd, SarlHtmlDocletOptions.TITLE_OPTION, title);

		// Special tags
		if (!docconfig.getEnableDeprecatedTag()) {
			cmd.add(SarlHtmlDocletOptions.NODEPRECATED_OPTION);
		}

		if (!docconfig.getEnableSinceTag()) {
			cmd.add(SarlHtmlDocletOptions.NOSINCE_OPTION);
		}

		if (docconfig.getEnableVersionTag()) {
			cmd.add(SarlHtmlDocletOptions.VERSION_OPTION);
		}

		if (docconfig.getEnableAuthorTag()) {
			//cmd.add(SarlHtmlDocletOptions.AUTHOR_OPTION);
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

		// Add custom tags
		for (final var tag : docconfig.getCustomTags()) {
			addCmd(cmd, TAG_FLAG, tag.toString());
		}

		// Doclet
		var docletName = docconfig.getDoclet();
		final Class<?> docletType;
		if (Strings.isNullOrEmpty(docletName)) {
			docletType = Doclet.class;
		} else {
			try {
				docletType = getClass().getClassLoader().loadClass(docletName);
			} catch (ClassNotFoundException exception) {
				return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
			}
		}

		// Javadoc Options
		for (final var option : docconfig.getJavadocOption()) {
			cmd.add(option);
		}

		// Path detection
		final var paths = this.pathDetector.get();
		if (!paths.isResolved()) {
			paths.setSarlOutputPath(cconfig.getOutputPath());
			paths.setClassOutputPath(cconfig.getClassOutputPath());
			paths.setTempDirectory(cconfig.getTempDirectory());
			paths.setDocumentationOutputPath(docconfig.getDocumentationOutputDirectory());
			try {
				paths.resolve(cli.standaloneArguments());
			} catch (IOException exception) {
				return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
			}
		}

		// Add the flags for the doclet output folder
		addCmd(cmd, SarlHtmlDocletOptions.LONG_DIRECTORY_OPTION, paths.getDocumentationOutputPath().getAbsolutePath());

		// Source folder
		final var sourcePath = new SystemPath();
		for (final var path : cli.standaloneArguments()) {
			sourcePath.add(path);
		}
		if (paths.getSarlOutputPath() != null) {
			sourcePath.add(paths.getSarlOutputPath());
		}

		// Add source files
		final var files = getSourceFiles(sourcePath, docconfig);
		
		// Execute the Javadoc
		return runJavadoc(
				files,
				paths,
				docletType,
				cmd,
				docconfig,
				logger,
				errorCount, warningCount);
	}

	/** Run the Javadoc binary based on the provided arguments.
	 *
	 * @param sourceFiles the list of the Java source folders or files.
	 * @param paths the definition of the paths for the documentation tool.
	 * @param docletClass the type of the doclet to be used.
	 * @param javadocOptions the list of the options to pass to the Javadoc tool.
	 * @param docconfig the configuration of the sarldoc tool.
	 * @param logger the logger to be used for ontifiy the user.
	 * @param errorCount the number of errors found into the project.
	 * @param warningCount the number of warnings found into the project.
	 * @return the result of the execution.
	 */
	protected abstract CommandOutcome runJavadoc(
			Collection<File> sourceFiles,
			DocumentationPathDetector paths,
			Class<?> docletClass,
			List<String> javadocOptions,
			SarldocConfig docconfig,
			Logger logger,
			AtomicInteger errorCount,
			AtomicInteger warningCount);

	private static boolean isExcludedPackage(File file, SarldocConfig config) {
		if (file == null) {
			return true;
		}
		final var excl = config.getExcludedPackages();
		if (excl.isEmpty()) {
			return false;
		}
		final var name = new StringBuilder();
		var cfile = file.getParentFile();
		var first = true;
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

	private static Collection<File> getSourceFiles(SystemPath sourcePaths, SarldocConfig config) {
		final var allFiles = new TreeSet<File>();
		final var pathTraverser = new PathTraverser();
		final var pathes = pathTraverser.resolvePathes(
			sourcePaths.toFilenameList(),
			input -> Objects.equals(JAVA_FILE_EXTENSION, input.fileExtension()));
		for (final var entry : pathes.entries()) {
			final var filename = entry.getValue().toFileString();
			final var file = FileSystem.convertStringToFile(filename);
			final var root = FileSystem.convertStringToFile(entry.getKey());
			try {
				if (!isExcludedPackage(FileSystem.makeRelative(file, root), config)) {
					allFiles.add(file);
				}
			} catch (Throwable exception) {
				// Silent exception
			}
		}
		return allFiles;
	}

}
