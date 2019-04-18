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
import java.net.URI;
import java.text.MessageFormat;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.Set;
import java.util.TreeSet;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.collect.Multimap;
import com.google.inject.Provider;
import io.bootique.cli.Cli;
import io.bootique.command.CommandManager;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.command.ManagedCommand;
import io.bootique.meta.application.CommandMetadata;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.lang3.SystemUtils;
import org.arakhne.afc.vmutil.FileSystem;
import org.eclipse.xtext.mwe.PathTraverser;
import org.slf4j.Logger;

import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.lang.sarlc.tools.PathDetector;
import io.sarl.lang.sarlc.tools.SARLBootClasspathProvider;
import io.sarl.sarldoc.configs.SarlConfig;
import io.sarl.sarldoc.configs.subconfigs.SarldocConfig;
import io.sarl.sarldoc.utils.SystemPath;

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

	private final Provider<SarlConfig> config;

	private final Provider<SARLBootClasspathProvider> defaultBootClasspath;

	private final Provider<PathDetector> pathDetector;

	private final Logger logger;

	/** Constructor.
	 *
	 * @param commandManager the provider of the manager of the commands.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 * @param defaultBootClasspath the provider of default boot classpath.
	 * @param pathDetector the detector of paths.
	 */
	public SarldocCommand(Provider<CommandManager> commandManager, Logger logger, Provider<SarlConfig> config,
			Provider<SARLBootClasspathProvider> defaultBootClasspath, Provider<PathDetector> pathDetector) {
		super(CommandMetadata
				.builder(SarldocCommand.class)
				.description(Messages.SarldocCommand_0));
		this.commandManagerProvider = commandManager;
		this.logger = logger;
		this.config = config;
		this.defaultBootClasspath = defaultBootClasspath;
		this.pathDetector = pathDetector;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		final SarlConfig genconfig = this.config.get();
		CommandOutcome outcome = runSarlc(cli);
		if (outcome.isSuccess()) {
			runJavadoc(cli, genconfig);
		}
		return outcome;
	}

	private CommandOutcome runSarlc(Cli cli) {
		CommandOutcome outcome;
		try {
			this.logger.info(Messages.SarldocCommand_1);
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
	private static String parseJavadocMemory(String memory) throws IllegalArgumentException {
		if (Strings.isNullOrEmpty(memory)) {
			return null;
		}

		Pattern p = Pattern.compile("^\\s*(\\d+)\\s*?\\s*$"); //$NON-NLS-1$
		Matcher m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "m"; //$NON-NLS-1$
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*k(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "k"; //$NON-NLS-1$
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*m(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "m"; //$NON-NLS-1$
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*g(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		m = p.matcher(memory);
		if (m.matches()) {
			return (Integer.parseInt(m.group(1)) * 1024) + "m"; //$NON-NLS-1$
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*t(b)?\\s*$", Pattern.CASE_INSENSITIVE); //$NON-NLS-1$
		m = p.matcher(memory);
		if (m.matches()) {
			return ( Integer.parseInt(m.group(1)) * 1024 * 1024) + "m"; //$NON-NLS-1$
		}

		throw new IllegalArgumentException(MessageFormat.format(Messages.SarldocCommand_3, memory));
	}

	private static boolean addJOption(CommandLine cmd, String arg) throws IllegalArgumentException {
		final String opt = mergeIfNotNull("-J", arg); //$NON-NLS-1$
		if (!Strings.isNullOrEmpty(opt)) {
			cmd.addArgument(opt);
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
		String host = System.getProperty(hostVar, null);
		if (!Strings.isNullOrEmpty(host)) {
			String port = System.getProperty(portVar, null);
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
		String host = SystemUtils.getEnvironmentVariable(var, null);
		if (!Strings.isNullOrEmpty(host)) {
			try {
				final URI uri = new URI(host);
				activeProxies.putIfAbsent(protocol, uri);
			} catch (Throwable exception) {
				// Silent error
			}
		}
	}

	private static void addProxyArg(CommandLine cmd, SarldocConfig config) {
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

	private static String or(String a, String b) {
		if (Strings.isNullOrEmpty(a)) {
			return b;
		}
		return a;
	}

	private CommandOutcome runJavadoc(Cli cli, SarlConfig genconfig) throws IllegalArgumentException {
		this.logger.info(Messages.SarldocCommand_2);
		final SarldocConfig docconfig = genconfig.getSarldoc();
		String javadocExecutable = docconfig.getJavadocExecutable();
		CommandLine cmd = new CommandLine(javadocExecutable);

		// Memory Options
		addJOption( cmd, mergeIfNotNull("-Xmx", parseJavadocMemory(docconfig.getMaxMemory()))); //$NON-NLS-1$
		addJOption( cmd, mergeIfNotNull("-Xms", parseJavadocMemory(docconfig.getMinMemory()))); //$NON-NLS-1$

		// Proxy Options
		addProxyArg(cmd, docconfig);

		// -J Options
		for (final String option : docconfig.getJOption()) {
			addJOption(cmd, option);
		}

		// Javadoc Options
		for (final String option : docconfig.getJavadocOption()) {
			cmd.addArgument(option);
		}
		
		// Java version
		final String javaVersion = genconfig.getCompiler().getJavaVersion();
		if (!Strings.isNullOrEmpty(javaVersion)) {
			cmd.addArgument("-source").addArgument(javaVersion); //$NON-NLS-1$
		}

		// Standard Doclet Options
		final String title = docconfig.getTitle();
		if (!Strings.isNullOrEmpty(title)) {
			cmd.addArgument("-doctitle").addArgument(title); //$NON-NLS-1$
		}

		if (!docconfig.getEnableDeprecatedTag()) {
			cmd.addArgument("-nodeprecated").addArgument("-nodeprecatedlist"); //$NON-NLS-1$//$NON-NLS-2$
		}

		if (!docconfig.getEnableSinceTag()) {
			cmd.addArgument("-nosince"); //$NON-NLS-1$
		}

		if (docconfig.getEnableVersionTag()) {
			cmd.addArgument("-version"); //$NON-NLS-1$
		}

		if (docconfig.getEnableAuthorTag()) {
			cmd.addArgument("-author"); //$NON-NLS-1$
		}

		cmd.addArgument("-docencoding").addArgument(docconfig.getEncoding()); //$NON-NLS-1$

		// Visibility of the elements
		switch (docconfig.getVisibility()) {
		case PUBLIC:
			cmd.addArgument("-public"); //$NON-NLS-1$
			break;
		case PROTECTED:
			cmd.addArgument("-protected"); //$NON-NLS-1$
			break;
		case PACKAGE:
			cmd.addArgument("-package"); //$NON-NLS-1$
			break;
		case PRIVATE:
			cmd.addArgument("-private"); //$NON-NLS-1$
			break;
		default:
			throw new IllegalStateException();
		}

		// Path detection
		final PathDetector paths = this.pathDetector.get();
		paths.setSarlOutputPath(genconfig.getOutputPath());
		paths.setClassOutputPath(genconfig.getClassOutputPath());
		paths.setWorkingPath(genconfig.getWorkingPath());
		paths.resolve(cli.standaloneArguments());

		// Source folder
		final SystemPath sourcePath = new SystemPath();
		for (final String path : cli.standaloneArguments()) {
			sourcePath.add(path);
		}
		if (paths.getSarlOutputPath() != null) {
			sourcePath.add(paths.getSarlOutputPath());
		}
		cmd.addArgument("-sourcepath").addArgument(sourcePath.toString()); //$NON-NLS-1$

		// Class path
		final SystemPath fullClassPath = new SystemPath();
		fullClassPath.add(or(genconfig.getBootClasspath(), this.defaultBootClasspath.get().getClasspath()));
		fullClassPath.add(genconfig.getClasspath());
		fullClassPath.add(paths.getClassOutputPath());
		cmd.addArgument("-classpath").addArgument(fullClassPath.toString()); //$NON-NLS-1$

		// Output folder
		
		cmd.addArgument("-d").addArgument(docconfig.getOutputDirectory().getAbsolutePath()); //$NON-NLS-1$

		// Doclet
		cmd.addArgument("-doclet").addArgument(docconfig.getDoclet()); //$NON-NLS-1$
		cmd.addArgument("-docletpath").addArgument(docconfig.getDocletPath()); //$NON-NLS-1$

		// Add source files
		final Collection<String> files = getSourceFiles(sourcePath, docconfig);
		for (final String sourceFile : files) {
			cmd.addArgument(sourceFile);
		}
		
		// Execute the Javadoc
		final DefaultExecutor executor = new DefaultExecutor();
		try {
			final int code = executor.execute(cmd);

			if (code == 0) {
				return CommandOutcome.succeeded();
			}
			return CommandOutcome.failed(Constants.ERROR_CODE, ""); //$NON-NLS-1$
		} catch (IOException exception) {
			return CommandOutcome.failed(Constants.ERROR_CODE, exception);
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
		File f = file.getParentFile();
		boolean first = true;
		while (f != null && !Objects.equals(f.getName(), FileSystem.CURRENT_DIRECTORY) && !Objects.equals(f.getName(), FileSystem.PARENT_DIRECTORY)) {
			if (first) {
				first = false;
			} else {
				name.insert(0, "."); //$NON-NLS-1$
			}
			name.insert(0, f.getName());
			f = f.getParentFile();
		}
		return !excl.contains(name.toString());
	}

	private static Collection<String> getSourceFiles(SystemPath sourcePaths, SarldocConfig config) {
		final Set<String> allFiles = new TreeSet<>();
		final PathTraverser pathTraverser = new PathTraverser();
		final Multimap<String, org.eclipse.emf.common.util.URI> pathes = pathTraverser.resolvePathes(
				sourcePaths.getElements(),
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

}
