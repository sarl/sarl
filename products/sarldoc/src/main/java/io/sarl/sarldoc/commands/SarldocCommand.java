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

import java.io.IOException;
import java.net.Proxy;
import java.net.URI;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.inject.Provider;
import io.bootique.cli.Cli;
import io.bootique.command.CommandManager;
import io.bootique.command.CommandOutcome;
import io.bootique.command.CommandWithMetadata;
import io.bootique.command.ManagedCommand;
import io.bootique.meta.application.CommandMetadata;
import org.apache.commons.exec.CommandLine;
import org.apache.commons.exec.DefaultExecutor;
import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;

import io.sarl.lang.sarlc.Constants;
import io.sarl.lang.sarlc.commands.CompilerCommand;
import io.sarl.sarldoc.configs.SarlConfig;
import io.sarl.sarldoc.configs.subconfigs.SarldocConfig;

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

	private final Logger logger;

	/** Constructor.
	 *
	 * @param commandManager the provider of the manager of the commands.
	 * @param logger the logger to be used by the command.
	 * @param config the sarldoc configuration provider.
	 */
	public SarldocCommand(Provider<CommandManager> commandManager, Logger logger, Provider<SarlConfig> config) {
		super(CommandMetadata
				.builder(SarldocCommand.class)
				.description(Messages.SarldocCommand_0));
		this.commandManagerProvider = commandManager;
		this.logger = logger;
		this.config = config;
	}

	@Override
	public CommandOutcome run(Cli cli) {
		CommandOutcome outcome = runSarlc(cli);
		if (outcome.isSuccess()) {
			runJavadoc(cli);
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
			throw new IllegalArgumentException("The memory could not be null.");
		}

		Pattern p = Pattern.compile("^\\s*(\\d+)\\s*?\\s*$");
		Matcher m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "m";
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*k(b)?\\s*$", Pattern.CASE_INSENSITIVE);
		m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "k";
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*m(b)?\\s*$", Pattern.CASE_INSENSITIVE);
		m = p.matcher(memory);
		if (m.matches()) {
			return m.group(1) + "m";
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*g(b)?\\s*$", Pattern.CASE_INSENSITIVE);
		m = p.matcher(memory);
		if (m.matches()) {
			return (Integer.parseInt(m.group(1)) * 1024) + "m";
		}

		p = Pattern.compile("^\\s*(\\d+)\\s*t(b)?\\s*$", Pattern.CASE_INSENSITIVE);
		m = p.matcher(memory);
		if (m.matches()) {
			return ( Integer.parseInt(m.group(1)) * 1024 * 1024) + "m";
		}

		throw new IllegalArgumentException("Could convert not to a memory size: " + memory);
	}

	private void addJOption(CommandLine cmd, String arg) throws IllegalArgumentException {
		if (!Strings.isNullOrEmpty(arg)) {
			cmd.addArgument("-J" + arg);
		}
	}

	private void addProxyArg(CommandLine cmd, SarldocConfig config) {
		final Map<String, URI> activeProxies = new HashMap<>();

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

		URI uri = activeProxies.get("https");
		if (uri != null) {
			addJOption(cmd, "-Dhttps.proxyHost=" + uri.getHost());
			addJOption(cmd, "-Dhttps.proxyPort=" + uri.getPort());
		}

		uri = activeProxies.get("http");
		if (uri != null) {
			addJOption(cmd, "-Dhttp.proxyHost=" + uri.getHost());
			addJOption(cmd, "-Dhttp.proxyPort=" + uri.getPort());
		}
	}

	private CommandOutcome runJavadoc(Cli cli) throws IllegalArgumentException {
		this.logger.info(Messages.SarldocCommand_2);
		final SarldocConfig docconfig = this.config.get().getSarldoc();
		String javadocExecutable = docconfig.getJavadocExecutable();
		CommandLine cmd = new CommandLine(javadocExecutable);

		addJOption( cmd, "-Xmx" + parseJavadocMemory(docconfig.getMaxMemory()));
		addJOption( cmd, "-Xms" + parseJavadocMemory(docconfig.getMinMemory()));

		addProxyArg(cmd, docconfig);

		for (final String option : docconfig.getJOption()) {
			addJOption(cmd, option);
		}

		// Wrap Standard doclet Options
		cmd.addArgument("-doclet").addArgument(docconfig.getDoclet());

		// ----------------------------------------------------------------------
		// Wrap Javadoc options
		// ----------------------------------------------------------------------
		List<String> javadocArguments = new ArrayList<>();

		addJavadocOptions( javadocOutputDirectory, javadocArguments, sourcePaths, offlineLinks );

		// ----------------------------------------------------------------------
		// Write options file and include it in the command line
		// ----------------------------------------------------------------------

		List<String> arguments = new ArrayList<>( javadocArguments.size() + standardDocletArguments.size() );
		arguments.addAll( javadocArguments );
		arguments.addAll( standardDocletArguments );

		if ( arguments.size() > 0 )
		{
			addCommandLineOptions( cmd, arguments, javadocOutputDirectory );
		}

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


}
