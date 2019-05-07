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

package io.sarl.sarldoc.commands;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.Writer;
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

import com.google.common.base.Strings;
import com.google.common.collect.Multimap;
import com.google.inject.Provider;
import com.sun.tools.doclets.standard.Standard;
import com.sun.tools.javadoc.Main;
import io.bootique.cli.Cli;
import io.bootique.command.CommandManager;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.command.ManagedCommand;
import io.bootique.meta.application.CommandMetadata;
import org.apache.commons.lang3.SystemUtils;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.mwe.PathTraverser;
import org.slf4j.Logger;

import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.configs.SarlcConfig;
import io.sarl.lang.sarlc.tools.ClassPathUtils;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLClasspathProvider;
import io.sarl.maven.bootiqueapp.BootiqueMain;
import io.sarl.maven.bootiqueapp.utils.SystemPath;
import io.sarl.sarldoc.configs.SarldocConfig;
import io.sarl.sarldoc.configs.Tag;

/**
 * Command for launching sarldoc.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.10
 */
public class SarldocCommand extends CommandWithMetadata {

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

	private static final String EMPTY_STRING = ""; //$NON-NLS-1$

	private static final String PACKAGE_SEPARATOR = "."; //$NON-NLS-1$

	private static final String JAVA_FILE_EXTENSION = "java"; //$NON-NLS-1$

	private static final String JAVADOC_NOTE_PREFIX = "Note:";  //$NON-NLS-1$

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

	private static final String SMALLD_FLAG = "-d"; //$NON-NLS-1$

	private static final String DOCLET_FLAG = "-doclet"; //$NON-NLS-1$

	private static final String DOCLETPATH_FLAG = "-docletpath"; //$NON-NLS-1$

	private static final String TAG_FLAG = "-tag"; //$NON-NLS-1$

	private final Provider<CommandManager> commandManagerProvider;

	private final Provider<SarldocConfig> config;

	private final Provider<SarlcConfig> sarlcConfig;

	private final Provider<SARLClasspathProvider> defaultClasspath;

	private final Provider<PathDetector> pathDetector;

	private final Provider<Logger> logger;

	/** Constructor.
	 *
	 * @param commandManager the provider of the manager of the commands.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param sarlcConfig the sarlc configuration provider.
	 * @param defaultClasspath the provider of default classpaths.
	 * @param pathDetector the detector of paths.
	 */
	public SarldocCommand(Provider<CommandManager> commandManager, Provider<Logger> logger, Provider<SarldocConfig> config,
			Provider<SarlcConfig> sarlcConfig, Provider<SARLClasspathProvider> defaultClasspath,
			Provider<PathDetector> pathDetector) {
		super(CommandMetadata
				.builder(SarldocCommand.class)
				.description(Messages.SarldocCommand_0));
		this.commandManagerProvider = commandManager;
		this.logger = logger;
		this.config = config;
		this.sarlcConfig = sarlcConfig;
		this.defaultClasspath = defaultClasspath;
		this.pathDetector = pathDetector;
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

	private CommandOutcome runSarlc(Cli cli, Logger logger) {
		CommandOutcome outcome;
		try {
			logger.info(Messages.SarldocCommand_1);
			final CommandManager cmdManager = this.commandManagerProvider.get();
			final ManagedCommand compilerCommand = cmdManager.lookupByType(CompilerCommand.class);
			outcome = compilerCommand.getCommand().run(cli);
		} catch (Exception e) {
			outcome = CommandOutcome.failed(1, e);
		}
		return outcome;
	}

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
		final String host = SystemUtils.getEnvironmentVariable(var, null);
		if (!Strings.isNullOrEmpty(host)) {
			try {
				final URI uri = new URI(host);
				activeProxies.putIfAbsent(protocol, uri);
			} catch (Throwable exception) {
				// Silent error
			}
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
			logger.debug(MessageFormat.format(Messages.SarldocCommand_9, HTTP_PROTOCOL_NAME, httpNoProxy));
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
			logger.debug(MessageFormat.format(Messages.SarldocCommand_9, HTTPS_PROTOCOL_NAME, httpsNoProxy));
			System.setProperty(HTTPS_NONPROXYHOST_PROPERTY_NAME, httpsNoProxy);
		}
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
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
		final String javaVersion = cconfig.getCompiler().getJavaVersion();
		if (!Strings.isNullOrEmpty(javaVersion)) {
			cmd.add(SOURCE_FLAG);
			cmd.add(javaVersion);
		}

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
		paths.setWorkingPath(cconfig.getWorkingPath());
		paths.resolve(cli.standaloneArguments());

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
		final SystemPath fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cconfig, logger);
		cmd.add(CLASSPATH_FLAG);
		cmd.add(fullClassPath.toString());

		// Output folder
		cmd.add(SMALLD_FLAG);
		cmd.add(docconfig.getOutputDirectory().getAbsolutePath());

		// Doclet
		cmd.add(DOCLET_FLAG);
		cmd.add(docconfig.getDoclet());
		cmd.add(DOCLETPATH_FLAG);
		cmd.add(docconfig.getDocletPath());

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
		try {
			// Delete target directory
			logger.info(MessageFormat.format(Messages.SarldocCommand_5, docconfig.getOutputDirectory().getAbsolutePath()));
			FileSystem.delete(docconfig.getOutputDirectory().getAbsoluteFile());

			// Run Javadoc
			final String[] args = new String[cmd.size()];
			cmd.toArray(args);
			try (PrintWriter errWriter = new PrintWriter(new ErrorWriter(logger, errorCount))) {
				try (PrintWriter warnWriter = new PrintWriter(new WarningWriter(logger, warningCount))) {
					try (PrintWriter infoWriter = new PrintWriter(new InformationWriter(logger, warningCount))) {
						final int code = Main.execute(
								javadocExecutable,
								errWriter,
								warnWriter,
								infoWriter,
								Standard.class.getName(),
								args);
						if (code == 0) {
							return CommandOutcome.succeeded();
						}
						return CommandOutcome.failed(BootiqueMain.ERROR_CODE, EMPTY_STRING);
					}
				}
			}
		} catch (IOException exception) {
			return CommandOutcome.failed(BootiqueMain.ERROR_CODE, exception);
		}
	}

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

	/** Print writer that is able to output message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private abstract static class LogWriter extends Writer {

		/** Logger.
		 */
		protected final Logger logger;

		/** Number of errors.
		 */
		protected final AtomicInteger errorCount;

		/** Number of errors.
		 */
		protected final AtomicInteger warningCount;

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param errorCount the counter of errors.
		 * @param warningCount the counter of warnings.
		 */
		LogWriter(Logger logger, AtomicInteger errorCount, AtomicInteger warningCount) {
			this.logger = logger;
			this.errorCount = errorCount;
			this.warningCount = warningCount;
		}

		@Override
		public final void flush() throws IOException {
			//
		}

		@Override
		public final void close() throws IOException {
			//
		}

		@Override
		public final void write(char[] cbuf, int off, int len) throws IOException {
			String message = new String(cbuf, off, len);
			message = message.trim();
			if (!Strings.isNullOrEmpty(message)) {
				log(message);
			}
		}

		protected abstract void log(String message);

	}

	/** Print writer that is able to output information with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class InformationWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param warningCount the counter of warnings.
		 */
		InformationWriter(Logger logger, AtomicInteger warningCount) {
			super(logger, null, warningCount);
		}

		@Override
		protected void log(String message) {
			if (message.startsWith(JAVADOC_NOTE_PREFIX)) {
				this.logger.warn(message.substring(JAVADOC_NOTE_PREFIX.length()).trim());
				this.warningCount.incrementAndGet();
			} else {
				this.logger.info(message);
			}
		}

	}

	/** Print writer that is able to output warning message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class WarningWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param warningCount the counter of warnings.
		 */
		WarningWriter(Logger logger, AtomicInteger warningCount) {
			super(logger, null, warningCount);
		}

		@Override
		protected void log(String message) {
			this.logger.warn(message);
			this.warningCount.incrementAndGet();
		}

	}

	/** Print writer that is able to output error message with the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static class ErrorWriter extends LogWriter {

		/** Constructor.
		 *
		 * @param logger the logger.
		 * @param errorCount the counter of errors.
		 */
		ErrorWriter(Logger logger, AtomicInteger errorCount) {
			super(logger, errorCount, null);
		}

		@Override
		protected void log(String message) {
			this.logger.error(message);
			this.errorCount.incrementAndGet();
		}

	}

}
