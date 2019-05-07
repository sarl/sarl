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
import java.util.regex.Matcher;
import java.util.regex.Pattern;

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
		CommandOutcome outcome = runSarlc(cli, logger);
		if (outcome.isSuccess()) {
			final SarldocConfig dconfig = this.config.get();
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

	/**
	 * Parse a memory string which be used in the JVM arguments <code>-Xms</code> or <code>-Xmx</code>. <br>
	 * Here are some supported memory string depending the JDK used:
	 * <table summary="Memory argument support per JDK">
	 * <tr>
	 * <th>JDK</th>
	 * <th>Memory argument support for <code>-Xms</code> or <code>-Xmx</code></th>
	 * </tr>
	 * <tr>
	 * <td>SUN</td>
	 * <td>1024k | 128m | 1g | 1t</td>
	 * </tr>
	 * <tr>
	 * <td>IBM</td>
	 * <td>1024k | 1024b | 128m | 128mb | 1g | 1gb</td>
	 * </tr>
	 * <tr>
	 * <td>BEA</td>
	 * <td>1024k | 1024kb | 128m | 128mb | 1g | 1gb</td>
	 * </tr>
	 * </table>
	 *
	 * @param memory the memory to be parsed, not null.
	 * @return the memory parsed with a supported unit. If no unit specified in the <code>memory</code> parameter, the
	 *         default unit is <code>m</code>. The units <code>g | gb</code> or <code>t | tb</code> will be converted in
	 *         <code>m</code>.
	 * @throws IllegalArgumentException if the <code>memory</code> parameter is null or doesn't match any pattern.
	 */
	@SuppressWarnings("checkstyle:magicnumber")
	private static String parseJavadocMemory(String memory) throws IllegalArgumentException {
		if (Strings.isNullOrEmpty(memory)) {
			return null;
		}

		Pattern pattern = Pattern.compile("^\\s*(\\d+)\\s*?\\s*$"); //$NON-NLS-1$
		Matcher matcher = pattern.matcher(memory);
		if (matcher.matches()) {
			return matcher.group(1) + "m"; //$NON-NLS-1$
		}

		pattern = Pattern.compile("^\\s*(\\d+)\\s*k(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		matcher = pattern.matcher(memory);
		if (matcher.matches()) {
			return matcher.group(1) + "k"; //$NON-NLS-1$
		}

		pattern = Pattern.compile("^\\s*(\\d+)\\s*m(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		matcher = pattern.matcher(memory);
		if (matcher.matches()) {
			return matcher.group(1) + "m"; //$NON-NLS-1$
		}

		pattern = Pattern.compile("^\\s*(\\d+)\\s*g(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		matcher = pattern.matcher(memory);
		if (matcher.matches()) {
			return (Integer.parseInt(matcher.group(1)) * 1024) + "m"; //$NON-NLS-1$
		}

		pattern = Pattern.compile("^\\s*(\\d+)\\s*t(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		matcher = pattern.matcher(memory);
		if (matcher.matches()) {
			return (Integer.parseInt(matcher.group(1)) * 1024 * 1024) + "m"; //$NON-NLS-1$
		}

		throw new IllegalArgumentException(MessageFormat.format(Messages.SarldocCommand_3, memory));
	}

	private static boolean addJOption(List<String> cmd, String arg) throws IllegalArgumentException {
		final String opt = mergeIfNotNull("-J", arg); //$NON-NLS-1$
		if (!Strings.isNullOrEmpty(opt)) {
			cmd.add(opt);
			return true;
		}
		return false;
	}

	private static String mergeIfNotNull(String base, String value) {
		if (!Strings.isNullOrEmpty(value)) {
			return base + value;
		}
		return null;
	}

	private static void addProxyFromProperty(Map<String, URI> activeProxies, String protocol, String hostVar, String portVar) {
		final String host = System.getProperty(hostVar, null);
		if (!Strings.isNullOrEmpty(host)) {
			final String port = System.getProperty(portVar, null);
			final URI uri;
			try {
				if (Strings.isNullOrEmpty(port)) {
					uri = new URI(protocol + "://" + host); //$NON-NLS-1$
				} else {
					uri = new URI(protocol + "://" + host + ":" + port); //$NON-NLS-1$ //$NON-NLS-2$
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

	@SuppressWarnings("checkstyle:npathcomplexity")
	private static void addProxyArg(List<String> cmd, SarldocConfig config) {
		final Map<String, URI> activeProxies = new HashMap<>();

		addProxyFromProperty(activeProxies, "http", "http.proxyHost", "http.proxyPort"); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
		addProxyFromProperty(activeProxies, "https", "https.proxyHost", "https.proxyPort"); //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$

		addProxyFromEnvironment(activeProxies, "http", "http_proxy"); //$NON-NLS-1$//$NON-NLS-2$
		addProxyFromEnvironment(activeProxies, "https", "https_proxy"); //$NON-NLS-1$//$NON-NLS-2$

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

		URI uri = activeProxies.get("https"); //$NON-NLS-1$
		boolean hasHttps = false;
		if (uri != null) {
			hasHttps = addJOption(cmd, mergeIfNotNull("-Dhttps.proxyHost=", uri.getHost())); //$NON-NLS-1$
			if (hasHttps && uri.getPort() != 0) {
				addJOption(cmd, "-Dhttps.proxyPort=" + uri.getPort()); //$NON-NLS-1$
			}
		}

		uri = activeProxies.get("http"); //$NON-NLS-1$
		if (uri != null) {
			final boolean hasHttp = addJOption(cmd, mergeIfNotNull("-Dhttp.proxyHost=", uri.getHost())); //$NON-NLS-1$
			if (hasHttp && uri.getPort() != 0) {
				addJOption(cmd, "-Dhttp.proxyPort=" + uri.getPort()); //$NON-NLS-1$
			}
			if (!hasHttps && hasHttp) {
				hasHttps = addJOption(cmd, mergeIfNotNull("-Dhttps.proxyHost=", uri.getHost())); //$NON-NLS-1$
				if (hasHttps && uri.getPort() != 0) {
					addJOption(cmd, "-Dhttps.proxyPort=" + uri.getPort()); //$NON-NLS-1$
				}
			}
		}
	}

	@SuppressWarnings("checkstyle:npathcomplexity")
	private CommandOutcome runJavadoc(Cli cli, SarldocConfig docconfig, SarlcConfig cconfig, Logger logger,
			AtomicInteger errorCount, AtomicInteger warningCount) throws IllegalArgumentException {
		logger.info(Messages.SarldocCommand_2);
		final String javadocExecutable = docconfig.getJavadocExecutable();
		final List<String> cmd = new ArrayList<>();

		// Locale
		cmd.add("-locale"); //$NON-NLS-1$
		cmd.add(docconfig.getLocale());

		// Encoding
		final String encoding = docconfig.getEncoding();
		cmd.add("-docencoding"); //$NON-NLS-1$
		cmd.add(encoding);
		cmd.add("-encoding"); //$NON-NLS-1$
		cmd.add(encoding);

		// Memory Options
		addJOption(cmd, mergeIfNotNull("-Xmx", parseJavadocMemory(docconfig.getMaxMemory()))); //$NON-NLS-1$
		addJOption(cmd, mergeIfNotNull("-Xms", parseJavadocMemory(docconfig.getMinMemory()))); //$NON-NLS-1$

		// Proxy Options
		addProxyArg(cmd, docconfig);

		// -J Options
		for (final String option : docconfig.getJOption()) {
			addJOption(cmd, option);
		}

		// Javadoc Options
		for (final String option : docconfig.getJavadocOption()) {
			cmd.add(option);
		}

		// Java version
		final String javaVersion = cconfig.getCompiler().getJavaVersion();
		if (!Strings.isNullOrEmpty(javaVersion)) {
			cmd.add("-source"); //$NON-NLS-1$
			cmd.add(javaVersion);
		}

		// Documentation title
		final String title = docconfig.getTitle();
		if (!Strings.isNullOrEmpty(title)) {
			cmd.add("-doctitle"); //$NON-NLS-1$
			cmd.add(title);
		}

		// Special tags
		if (!docconfig.getEnableDeprecatedTag()) {
			cmd.add("-nodeprecated"); //$NON-NLS-1$
			cmd.add("-nodeprecatedlist"); //$NON-NLS-1$
		}

		if (!docconfig.getEnableSinceTag()) {
			cmd.add("-nosince"); //$NON-NLS-1$
		}

		if (docconfig.getEnableVersionTag()) {
			cmd.add("-version"); //$NON-NLS-1$
		}

		if (docconfig.getEnableAuthorTag()) {
			cmd.add("-author"); //$NON-NLS-1$
		}

		// Visibility of the elements
		switch (docconfig.getVisibility()) {
		case PUBLIC:
			cmd.add("-public"); //$NON-NLS-1$
			break;
		case PROTECTED:
			cmd.add("-protected"); //$NON-NLS-1$
			break;
		case PACKAGE:
			cmd.add("-package"); //$NON-NLS-1$
			break;
		case PRIVATE:
			cmd.add("-private"); //$NON-NLS-1$
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
		cmd.add("-sourcepath"); //$NON-NLS-1$
		cmd.add(sourcePath.toString());

		// Class path
		final SARLClasspathProvider classpathProvider = this.defaultClasspath.get();
		final SystemPath fullClassPath = ClassPathUtils.buildClassPath(classpathProvider, cconfig, logger);
		cmd.add("-classpath"); //$NON-NLS-1$
		cmd.add(fullClassPath.toString());

		// Output folder
		cmd.add("-d"); //$NON-NLS-1$
		cmd.add(docconfig.getOutputDirectory().getAbsolutePath());

		// Doclet
		cmd.add("-doclet"); //$NON-NLS-1$
		cmd.add(docconfig.getDoclet());
		cmd.add("-docletpath"); //$NON-NLS-1$
		cmd.add(docconfig.getDocletPath());

		// Add custom tags
		for (final Tag tag : docconfig.getCustomTags()) {
			cmd.add("-tag"); //$NON-NLS-1$
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
						return CommandOutcome.failed(BootiqueMain.ERROR_CODE, ""); //$NON-NLS-1$
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
				name.insert(0, "."); //$NON-NLS-1$
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
				input -> Objects.equals("java", input.fileExtension())); //$NON-NLS-1$
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

		private static final String NOTE_PREFIX = "Note:"; //$NON-NLS-1$

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
			if (message.startsWith(NOTE_PREFIX)) {
				this.logger.warn(message.substring(NOTE_PREFIX.length()).trim());
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
