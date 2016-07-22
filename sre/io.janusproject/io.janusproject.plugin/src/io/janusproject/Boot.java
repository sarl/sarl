/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.janusproject;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.URL;
import java.util.Arrays;
import java.util.Iterator;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.inject.Module;
import com.hazelcast.logging.LoggingService;
import io.janusproject.kernel.Kernel;
import io.janusproject.services.network.NetworkConfig;
import io.janusproject.util.LoggerCreator;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.arakhne.afc.vmutil.locale.Locale;

import io.sarl.lang.core.Agent;

/**
 * This is the class that permits to boot the Janus platform.
 *
 * <p>
 * This class provides the "main" function for the platform. The list of the parameters is composed of a list of options, the
 * classname of an agent to launch, and the parameters to pass to the launched agent.
 *
 * <p>
 * The supported options may be obtain by passing no parameter, or the option <code>-h</code>.
 *
 * <p>
 * Example of Janus launching with Maven:
 * 
 * <pre>
 * <code>mvn exec:java
 *     -Dexec.mainClass="io.janusproject.Boot"
 *     -Dexec.args="my.Agent"</code>
 * </pre>
 *
 * <p>
 * Example of Janus launching from the CLI (only with the Jar file that is containing all the jar dependencies):
 * 
 * <pre>
 * <code>java -jar janus-with-dependencies.jar my.Agent</code>
 * </pre>
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class Boot {

	private static final int ERROR_EXIT_CODE = 255;

	private static PrintStream consoleLogger;

	private static Exiter applicationExiter;

	private Boot() {
		//
	}

	/**
	 * Parse the command line.
	 *
	 * @param args - the CLI arguments given to the program.
	 * @return the arguments that are not recognized as CLI options.
	 */
	@SuppressWarnings("checkstyle:cyclomaticcomplexity")
	public static String[] parseCommandLine(String[] args) {
		CommandLineParser parser = new DefaultParser();
		try {
			CommandLine cmd = parser.parse(getOptions(), args);

			// Show the help when there is no argument.
			if (cmd.getArgs().length == 0) {
				showHelp();
				return null;
			}

			boolean noLogo = false;
			int verbose = LoggerCreator.toInt(JanusConfig.VERBOSE_LEVEL_VALUE);

			Iterator<Option> optIterator = cmd.iterator();
			while (optIterator.hasNext()) {
				Option opt = optIterator.next();
				switch (opt.getLongOpt()) {
				case "help": //$NON-NLS-1$
					showHelp();
					return null;
				case "showdefaults": //$NON-NLS-1$
					showDefaults();
					return null;
				case "cli": //$NON-NLS-1$
					showCommandLineArguments(args);
					return null;
				case "file": //$NON-NLS-1$
					String rawFilename = opt.getValue();
					if (rawFilename == null || "".equals(rawFilename)) { //$NON-NLS-1$
						showHelp();
					}
					File file = new File(rawFilename);
					if (!file.canRead()) {
						showError(Locale.getString("INVALID_PROPERTY_FILENAME", //$NON-NLS-1$
								rawFilename), null);
						return null;
					}
					setPropertiesFrom(file);
					break;
				case "offline": //$NON-NLS-1$
					setOffline(true);
					break;
				case "randomid": //$NON-NLS-1$
					setRandomContextUUID();
					break;
				case "bootid": //$NON-NLS-1$
					setBootAgentTypeContextUUID();
					break;
				case "worldid": //$NON-NLS-1$
					setDefaultContextUUID();
					break;
				case "define": //$NON-NLS-1$
					String name = opt.getValue(0);
					if (!Strings.isNullOrEmpty(name)) {
						setProperty(name, Strings.emptyToNull(opt.getValue(1)));
					}
					break;
				case "log": //$NON-NLS-1$
					verbose = Math.max(LoggerCreator.toInt(opt.getValue()), 0);
					break;
				case "quiet": //$NON-NLS-1$
					if (verbose > 0) {
						--verbose;
					}
					break;
				case "verbose": //$NON-NLS-1$
					++verbose;
					break;
				case "nologo": //$NON-NLS-1$
					noLogo = true;
					break;
				default:
				}
			}

			// Change the verbosity
			setVerboseLevel(verbose);
			// Show the Janus logo?
			if (noLogo || verbose == 0) {
				setProperty(JanusConfig.JANUS_LOGO_SHOW_NAME, Boolean.FALSE.toString());
			}
			return cmd.getArgs();
		} catch (IOException | ParseException e) {
			showError(e.getLocalizedMessage(), e);
			// Event if showError never returns, add the return statement for
			// avoiding compilation error.
			return null;
		}
	}

	private static Class<? extends Agent> loadAgentClass(String fullyQualifiedName) {
		Class<?> type;
		try {
			type = Class.forName(fullyQualifiedName);
		} catch (Exception e) {
			showError(
					Locale.getString("INVALID_AGENT_QUALIFIED_NAME", //$NON-NLS-1$
							fullyQualifiedName, System.getProperty("java.class.path")), //$NON-NLS-1$
					e);
			// Event if showError never returns, add the return statement for
			// avoiding compilation error.
			return null;
		}
		// The following test is needed because the
		// cast to Class<? extends Agent> is not checking
		// the Agent type (it is a generic type, not
		// tested at runtime).
		if (Agent.class.isAssignableFrom(type)) {
			return type.asSubclass(Agent.class);
		}

		showError(Locale.getString("INVALID_AGENT_TYPE", //$NON-NLS-1$
				fullyQualifiedName), null);
		// Event if showError never returns, add the return statement for
		// avoiding compilation error.
		return null;
	}

	/**
	 * Main function that is parsing the command line and launching the first agent.
	 *
	 * @param args - command line arguments
	 * @see #startJanus(Class, Class, Object...)
	 */
	public static void main(String[] args) {
		try {
			Object[] freeArgs = parseCommandLine(args);
			if (JanusConfig.getSystemPropertyAsBoolean(JanusConfig.JANUS_LOGO_SHOW_NAME,
					JanusConfig.JANUS_LOGO_SHOW.booleanValue())) {
				showJanusLogo();
			}

			if (freeArgs.length == 0) {
				showError(Locale.getString("NO_AGENT_QUALIFIED_NAME"), //$NON-NLS-1$
						null);
				// Event if showError never returns, add the return statement for
				// avoiding compilation error.
				return;
			}

			String agentToLaunch = freeArgs[0].toString();
			freeArgs = Arrays.copyOfRange(freeArgs, 1, freeArgs.length, String[].class);

			// Load the agent class
			Class<? extends Agent> agent = loadAgentClass(agentToLaunch);
			assert (agent != null);

			startJanus((Class<? extends Module>) null, (Class<? extends Agent>) agent, freeArgs);
		} catch (Exception e) {
			showError(Locale.getString("LAUNCHING_ERROR", //$NON-NLS-1$
					e.getLocalizedMessage()), e);
			// Event if showError never returns, add the return statement for
			// avoiding compilation error.
			return;
		}
	}

	/**
	 * Replies the console stream for logging messages from the boot mechanism.
	 *
	 * <p>
	 * The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @return the console logger.
	 */
	public static PrintStream getConsoleLogger() {
		return consoleLogger == null ? System.err : consoleLogger;
	}

	/**
	 * Replies the console stream for logging messages from the boot mechanism.
	 *
	 * <p>
	 * The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @param stream - the stream to use for the console logging.
	 */
	public static void setConsoleLogger(PrintStream stream) {
		consoleLogger = stream;
	}

	/**
	 * Replies the command line options supported by this boot class.
	 *
	 * @return the command line options.
	 */
	public static Options getOptions() {
		Options options = new Options();

		options.addOption("B", "bootid", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_B", //$NON-NLS-1$
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));

		options.addOption("f", "file", true, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_F")); //$NON-NLS-1$

		options.addOption("h", "help", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_H")); //$NON-NLS-1$

		options.addOption(null, "nologo", false, //$NON-NLS-1$
				Locale.getString("CLI_HELP_NOLOGO")); //$NON-NLS-1$

		options.addOption("o", "offline", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_O", JanusConfig.OFFLINE)); //$NON-NLS-1$

		options.addOption("q", "quiet", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_Q")); //$NON-NLS-1$

		options.addOption("R", "randomid", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_R", //$NON-NLS-1$
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));

		options.addOption("s", "showdefaults", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_S")); //$NON-NLS-1$
		options.addOption(null, "cli", false, //$NON-NLS-1$
				Locale.getString("CLI_HELP_CLI")); //$NON-NLS-1$

		options.addOption("v", "verbose", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_V")); //$NON-NLS-1$

		options.addOption("W", "worldid", false, //$NON-NLS-1$//$NON-NLS-2$
				Locale.getString("CLI_HELP_W", //$NON-NLS-1$
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));
		StringBuilder b = new StringBuilder();
		int level = 0;
		for (String logLevel : LoggerCreator.getLevelStrings()) {
			if (b.length() > 0) {
				b.append(", "); //$NON-NLS-1$
			}
			b.append(logLevel);
			b.append(" ("); //$NON-NLS-1$
			b.append(level);
			b.append(")"); //$NON-NLS-1$
			++level;
		}
		Option opt = new Option("l", "log", true, Locale.getString("CLI_HELP_L", //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
				JanusConfig.VERBOSE_LEVEL_VALUE, b));
		opt.setArgs(1);
		options.addOption(opt);
		opt = new Option("D", "define", true, Locale.getString("CLI_HELP_D")); //$NON-NLS-1$//$NON-NLS-2$ //$NON-NLS-3$
		opt.setArgs(2);
		opt.setValueSeparator('=');
		opt.setArgName(Locale.getString("CLI_HELP_D_ARGNAME")); //$NON-NLS-1$
		options.addOption(opt);
		return options;
	}

	/**
	 * Show an error message, and exit.
	 *
	 * <p>
	 * This function never returns.
	 *
	 * @param message - the description of the error.
	 * @param exception - the cause of the error.
	 */
	@SuppressWarnings("checkstyle:regexp")
	protected static void showError(String message, Throwable exception) {
		try (PrintWriter logger = new PrintWriter(getConsoleLogger())) {
			if (message != null && !message.isEmpty()) {
				logger.println(message);
			}
			if (exception != null) {
				exception.printStackTrace(logger);
			}
			logger.flush();
			showHelp(logger);
		}
	}

	/**
	 * Show the help message on the standard console. This function never returns.
	 */
	public static void showHelp() {
		showHelp(new PrintWriter(getConsoleLogger()));
	}

	private static void showHelp(PrintWriter logger) {
		HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(logger, HelpFormatter.DEFAULT_WIDTH,
				Boot.class.getName() + " " //$NON-NLS-1$
						+ Locale.getString(Boot.class, "CLI_PARAM_SYNOPTIC"), //$NON-NLS-1$
				"", //$NON-NLS-1$
				getOptions(), HelpFormatter.DEFAULT_LEFT_PAD, HelpFormatter.DEFAULT_DESC_PAD, ""); //$NON-NLS-1$
		getExiter().exit();
	}

	/**
	 * Show the default values of the system properties. This function never returns.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static void showDefaults() {
		Properties defaultValues = new Properties();
		JanusConfig.getDefaultValues(defaultValues);
		NetworkConfig.getDefaultValues(defaultValues);
		try (OutputStream os = getConsoleLogger()) {
			defaultValues.storeToXML(os, null);
		} catch (Throwable e) {
			e.printStackTrace();
		}
		getExiter().exit();
	}

	/**
	 * Show the command line arguments. This function never returns.
	 *
	 * @param args - the command line arguments.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static void showCommandLineArguments(String[] args) {
		try (PrintStream os = getConsoleLogger()) {
			for (int i = 0; i < args.length; ++i) {
				os.println(i + ": " //$NON-NLS-1$
						+ args[i]);
			}
			os.flush();
		} catch (Throwable e) {
			e.printStackTrace();
		}
		getExiter().exit();
	}

	/**
	 * Show the heading logo of the Janus platform.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static void showJanusLogo() {
		System.out.println(Locale.getString("JANUS_TEXT_LOGO")); //$NON-NLS-1$
	}

	/**
	 * Set offline flag of the Janus platform.
	 *
	 * <p>
	 * This function is equivalent to the command line option <code>-o</code>.
	 *
	 * <p>
	 * This function must be called before launching the Janus platform.
	 *
	 * @param isOffline - the offline flag.
	 * @since 2.0.2.0
	 * @see JanusConfig#OFFLINE
	 */
	public static void setOffline(boolean isOffline) {
		System.setProperty(JanusConfig.OFFLINE, Boolean.toString(isOffline));
	}

	/**
	 * Force the Janus platform to use a random identifier for its default context.
	 *
	 * <p>
	 * This function is equivalent to the command line option <code>-R</code>.
	 *
	 * <p>
	 * This function must be called before launching the Janus platform.
	 *
	 * @since 2.0.2.0
	 * @see JanusConfig#BOOT_DEFAULT_CONTEXT_ID_NAME
	 * @see JanusConfig#RANDOM_DEFAULT_CONTEXT_ID_NAME
	 */
	public static void setRandomContextUUID() {
		System.setProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, Boolean.FALSE.toString());
		System.setProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME, Boolean.TRUE.toString());
	}

	/**
	 * Force the Janus platform to use a default context identifier that tis build upon the classname of the boot agent. It means
	 * that the UUID is always the same for a given classname.
	 *
	 * <p>
	 * This function is equivalent to the command line option <code>-B</code>.
	 *
	 * @since 2.0.2.0
	 * @see JanusConfig#BOOT_DEFAULT_CONTEXT_ID_NAME
	 * @see JanusConfig#RANDOM_DEFAULT_CONTEXT_ID_NAME
	 */
	public static void setBootAgentTypeContextUUID() {
		System.setProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, Boolean.TRUE.toString());
		System.setProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME, Boolean.FALSE.toString());
	}

	/**
	 * Force the Janus platform to use the identifier hard-coded in the source code for its default context.
	 *
	 * <p>
	 * This function is equivalent to the command line option <code>-W</code>.
	 *
	 * <p>
	 * This function must be called before launching the Janus platform.
	 *
	 * @since 2.0.2.0
	 * @see JanusConfig#BOOT_DEFAULT_CONTEXT_ID_NAME
	 * @see JanusConfig#RANDOM_DEFAULT_CONTEXT_ID_NAME
	 */
	public static void setDefaultContextUUID() {
		System.setProperty(JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, Boolean.FALSE.toString());
		System.setProperty(JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME, Boolean.FALSE.toString());
	}

	/**
	 * Force the verbosity level.
	 *
	 * <p>
	 * This function must be called before launching the Janus platform.
	 *
	 * @param level - the verbosity level.
	 * @since 2.0.2.0
	 * @see JanusConfig#VERBOSE_LEVEL_NAME
	 */
	public static void setVerboseLevel(int level) {
		System.setProperty(JanusConfig.VERBOSE_LEVEL_NAME, Integer.toString(level));
	}

	/**
	 * Set the system property. This function is an helper for setting a system property usually accessible with {@link System}.
	 *
	 * <p>
	 * This function must be called before launching the Janus platform.
	 *
	 * @param name - the name of the property.
	 * @param value - the value of the property. If the value is <code>null</code> or empty, the property is removed.
	 * @since 2.0.2.0
	 * @see System#setProperty(String, String)
	 * @see System#getProperties()
	 */
	public static void setProperty(String name, String value) {
		if (name != null && !name.isEmpty()) {
			if (value == null || value.isEmpty()) {
				System.getProperties().remove(name);
			} else {
				System.setProperty(name, value);
			}
		}
	}

	/**
	 * Set the system property from the content of the file with the given URL. This function is an helper for setting the system
	 * properties usually accessible with {@link System}.
	 *
	 * @param propertyFile - the URL from which a stream is opened.
	 * @throws IOException - if the stream cannot be read.
	 * @since 2.0.2.0
	 * @see System#getProperties()
	 * @see Properties#load(InputStream)
	 */
	public static void setPropertiesFrom(URL propertyFile) throws IOException {
		Properties systemProperties = System.getProperties();
		try (InputStream stream = propertyFile.openStream()) {
			systemProperties.load(stream);
		}
	}

	/**
	 * Set the system property from the content of the file with the given URL. This function is an helper for setting the system
	 * properties usually accessible with {@link System}.
	 *
	 * @param propertyFile - the URL from which a stream is opened.
	 * @throws IOException - if the stream cannot be read.
	 * @since 2.0.2.0
	 * @see System#getProperties()
	 * @see Properties#load(InputStream)
	 */
	public static void setPropertiesFrom(File propertyFile) throws IOException {
		Properties systemProperties = System.getProperties();
		try (InputStream stream = new FileInputStream(propertyFile)) {
			systemProperties.load(stream);
		}
	}

	/**
	 * Replies the identifier of the boot agent from the system's properties. The boot agent is launched with
	 * {@link #startJanus(Class, Class, Object...)}.
	 *
	 * @return the identifier of the boot agent, or <code>null</code> if it is unknown.
	 * @since 2.0.2.0
	 * @see JanusConfig#BOOT_AGENT_ID
	 * @see #startJanus(Class, Class, Object...)
	 */
	public static UUID getBootAgentIdentifier() {
		String id = JanusConfig.getSystemProperty(JanusConfig.BOOT_AGENT_ID);
		if (id != null && !id.isEmpty()) {
			try {
				return UUID.fromString(id);
			} catch (Throwable exception) {
				//
			}
		}
		return null;
	}

	/**
	 * Launch the Janus kernel and the first agent in the kernel.
	 *
	 * <p>
	 * Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * <p>
	 * The platformModule parameter permits to specify the injection module to use. The injection module is in change of
	 * creating/injecting all the components of the platform. The default injection module is retreived from the system property
	 * with the name stored in {@link JanusConfig#INJECTION_MODULE_NAME}. The default type for the injection module is stored in
	 * the constant {@link JanusConfig#INJECTION_MODULE_NAME_VALUE}.
	 *
	 * <p>
	 * The function {@link #getBootAgentIdentifier()} permits to retreive the identifier of the launched agent.
	 *
	 * @param platformModule - type of the injection module to use for initializing the platform, if <code>null</code> the default
	 *        module will be used.
	 * @param agentCls - type of the first agent to launch.
	 * @param params - parameters to pass to the agent as its initliazation parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #main(String[])
	 * @see #getBootAgentIdentifier()
	 */
	public static Kernel startJanus(Class<? extends Module> platformModule, Class<? extends Agent> agentCls, Object... params)
			throws Exception {
		Class<? extends Module> startupModule = platformModule;
		if (startupModule == null) {
			startupModule = JanusConfig.getSystemPropertyAsClass(Module.class, JanusConfig.INJECTION_MODULE_NAME,
					JanusConfig.INJECTION_MODULE_NAME_VALUE);
		}
		assert (startupModule != null) : "No platform injection module"; //$NON-NLS-1$
		return startJanus(startupModule.newInstance(), agentCls, params);
	}

	/**
	 * Launch the Janus kernel and the first agent in the kernel.
	 *
	 * <p>
	 * Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * <p>
	 * The startupModule parameter permits to specify the injection module to use. The injection module is in change of
	 * creating/injecting all the components of the platform. The default injection module is retreived from the system property
	 * with the name stored in {@link JanusConfig#INJECTION_MODULE_NAME}. The default type for the injection module is stored in
	 * the constant {@link JanusConfig#INJECTION_MODULE_NAME_VALUE}.
	 *
	 * <p>
	 * The function {@link #getBootAgentIdentifier()} permits to retreive the identifier of the launched agent.
	 *
	 * @param startupModule - the injection module to use for initializing the platform.
	 * @param agentCls - type of the first agent to launch.
	 * @param params - parameters to pass to the agent as its initliazation parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #main(String[])
	 * @see #getBootAgentIdentifier()
	 */
	public static Kernel startJanus(Module startupModule, Class<? extends Agent> agentCls, Object... params) throws Exception {
		// Set the boot agent classname
		System.setProperty(JanusConfig.BOOT_AGENT, agentCls.getName());
		// Get the start-up injection module
		assert (startupModule != null) : "No platform injection module"; //$NON-NLS-1$
		Kernel k = Kernel.create(startupModule);
		Logger logger = k.getLogger();
		if (logger != null) {
			logger.info(Locale.getString("LAUNCHING_AGENT", agentCls.getName())); //$NON-NLS-1$
		}
		UUID id = k.spawn(agentCls, params);
		if (id != null) {
			System.setProperty(JanusConfig.BOOT_AGENT_ID, id.toString());
		} else {
			System.getProperties().remove(JanusConfig.BOOT_AGENT_ID);
		}
		return k;
	}

	/**
	 * Replies the tool for exiting the application.
	 *
	 * @return the tool for exiting the application.
	 */
	public static Exiter getExiter() {
		return applicationExiter == null ? new StandardExiter() : applicationExiter;
	}

	/**
	 * Changes the tool that permits to stop the application.
	 *
	 * @param exiter - the exit tool.
	 */
	public static void setExiter(Exiter exiter) {
		applicationExiter = exiter;
	}

	/**
	 * Tool for exiting from the application.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface Exiter {

		/**
		 * Exit the application.
		 */
		void exit();

	}

	/**
	 * Tool for exiting from the application.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class StandardExiter implements Exiter {

		StandardExiter() {
			//
		}

		@Override
		public void exit() {
			System.exit(ERROR_EXIT_CODE);
		}

	}

}
