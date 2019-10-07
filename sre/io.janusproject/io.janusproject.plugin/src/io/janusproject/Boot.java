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

package io.janusproject;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.PrintStream;
import java.io.PrintWriter;
import java.net.MalformedURLException;
import java.net.URL;
import java.net.URLClassLoader;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import java.util.UUID;
import java.util.logging.Logger;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.inject.Module;
import com.hazelcast.logging.LoggingService;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.arakhne.afc.vmutil.FileSystem;
import org.arakhne.afc.vmutil.URISchemeType;
import org.eclipse.xtext.xbase.lib.Pure;

import io.janusproject.kernel.Kernel;
import io.janusproject.services.executor.EarlyExitException;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.network.NetworkConfig;
import io.janusproject.util.LoggerCreator;

import io.sarl.bootstrap.SRE;
import io.sarl.bootstrap.SREBootstrap;
import io.sarl.lang.SARLVersion;
import io.sarl.lang.core.Agent;

/**
 * This is the class that permits to boot the Janus platform.
 *
 * <p>This class provides the "main" function for the platform. The list of the parameters is composed of a list of options, the
 * classname of an agent to launch, and the parameters to pass to the launched agent.
 *
 * <p>The supported options may be obtain by passing no parameter, or the option <code>-h</code>.
 *
 * <p>Example of Janus launching with Maven:
 * <pre>
 * <code>mvn exec:java
 *     -Dexec.mainClass="io.janusproject.Boot"
 *     -Dexec.args="my.Agent"</code>
 * </pre>
 *
 * <p>Example of Janus launching from the CLI (only with the Jar file that is containing all the jar dependencies):
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

	/** Short command-line option for "embedded".
	 */
	public static final String CLI_OPTION_EMBEDDED_SHORT = "e"; //$NON-NLS-1$

	/** Long command-line option for "embedded".
	 */
	public static final String CLI_OPTION_EMBEDDED_LONG = "embedded"; //$NON-NLS-1$

	/** Short command-line option for "boot agent id".
	 */
	public static final String CLI_OPTION_BOOTID_SHORT = "B"; //$NON-NLS-1$

	/** Long command-line option for "boot agent id".
	 */
	public static final String CLI_OPTION_BOOTID_LONG = "bootid"; //$NON-NLS-1$

	/** Short command-line option for "random id".
	 */
	public static final String CLI_OPTION_RANDOMID_SHORT = "R"; //$NON-NLS-1$

	/** Long command-line option for "random id".
	 */
	public static final String CLI_OPTION_RANDOMID_LONG = "randomid"; //$NON-NLS-1$

	/** Short command-line option for "Janus world id".
	 */
	public static final String CLI_OPTION_WORLDID_SHORT = "W"; //$NON-NLS-1$

	/** Long command-line option for "Janus world id".
	 */
	public static final String CLI_OPTION_WORLDID_LONG = "worldid"; //$NON-NLS-1$

	/** Short command-line option for "file".
	 */
	public static final String CLI_OPTION_FILE_SHORT = "f"; //$NON-NLS-1$

	/** Long command-line option for "file".
	 */
	public static final String CLI_OPTION_FILE_LONG = "file"; //$NON-NLS-1$

	/** Short command-line option for "help".
	 */
	public static final String CLI_OPTION_HELP_SHORT = "h"; //$NON-NLS-1$

	/** Long command-line option for "help".
	 */
	public static final String CLI_OPTION_HELP_LONG = "help"; //$NON-NLS-1$

	/** Long command-line option for "nologo".
	 */
	public static final String CLI_OPTION_NOLOGO_LONG = "nologo"; //$NON-NLS-1$

	/** Short command-line option for "offline".
	 */
	public static final String CLI_OPTION_OFFLINE_SHORT = "o"; //$NON-NLS-1$

	/** Long command-line option for "offline".
	 */
	public static final String CLI_OPTION_OFFLINE_LONG = "offline"; //$NON-NLS-1$

	/** Short command-line option for "be quiet".
	 */
	public static final String CLI_OPTION_QUIET_SHORT = "q"; //$NON-NLS-1$

	/** Long command-line option for "be quiet".
	 */
	public static final String CLI_OPTION_QUIET_LONG = "quiet"; //$NON-NLS-1$

	/** Short command-line option for "be more verbose".
	 */
	public static final String CLI_OPTION_VERBOSE_SHORT = "v"; //$NON-NLS-1$

	/** Long command-line option for "be more verbose".
	 */
	public static final String CLI_OPTION_VERBOSE_LONG = "verbose"; //$NON-NLS-1$

	/** Long command-line option for "display the version".
	 */
	public static final String CLI_OPTION_VERSION = "version"; //$NON-NLS-1$

	/** Short command-line option for "change log level".
	 */
	public static final String CLI_OPTION_LOG_SHORT = "l"; //$NON-NLS-1$

	/** Long command-line option for "change log level".
	 */
	public static final String CLI_OPTION_LOG_LONG = "log"; //$NON-NLS-1$

	/** Short command-line option for "env. variable definition".
	 */
	public static final String CLI_OPTION_DEFINE_SHORT = "D"; //$NON-NLS-1$

	/** Long command-line option for "env. variable definition".
	 */
	public static final String CLI_OPTION_DEFINE_LONG = "define"; //$NON-NLS-1$

	/** Short command-line option for "show defaults".
	 */
	public static final String CLI_OPTION_SHOWDEFAULTS_SHORT = "s"; //$NON-NLS-1$

	/** Long command-line option for "show defaults".
	 */
	public static final String CLI_OPTION_SHOWDEFAULTS_LONG = "showdefaults"; //$NON-NLS-1$

	/** Short command-line option for "show the classpath".
	 */
	public static final String CLI_OPTION_SHOWCLASSPATH = "showclasspath"; //$NON-NLS-1$

	/** Short command-line option for "show CLI arguments".
	 */
	public static final String CLI_OPTION_SHOWCLIARGUMENTS_LONG = "cli"; //$NON-NLS-1$

	/** Short command-line option for "classpath".
	 */
	public static final String CLI_OPTION_CLASSPATH_SHORT = "cp"; //$NON-NLS-1$

	/** Long command-line option for "classpath".
	 */
	public static final String CLI_OPTION_CLASSPATH_LONG = "classpath"; //$NON-NLS-1$

	private static final int ERROR_EXIT_CODE = 255;

	private static final int SUCCESS_EXIT_CODE = 0;

	private static URLClassLoader dynamicClassLoader;

	private static PrintStream standardConsoleLogger;

	private static PrintStream errorConsoleLogger;

	private static Exiter applicationExiter;

	private Boot() {
		//
	}

	/**
	 * Parse the command line.
	 *
	 * @param args the CLI arguments given to the program.
	 * @return the arguments that are not recognized as CLI options.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static String[] parseCommandLine(String[] args) {
		final CommandLineParser parser = new DefaultParser();
		try {
			final CommandLine cmd = parser.parse(getOptions(), args, false);

			boolean noLogo = false;
			boolean embedded = false;
			boolean userClasspath = false;
			int verbose = LoggerCreator.toInt(JanusConfig.VERBOSE_LEVEL_VALUE);

			final Iterator<Option> optIterator = cmd.iterator();
			while (optIterator.hasNext()) {
				final Option opt = optIterator.next();
				String optName = opt.getLongOpt();
				if (Strings.isNullOrEmpty(optName)) {
					optName = opt.getOpt();
				}
				switch (optName) {
				case CLI_OPTION_HELP_LONG:
					showHelp(true);
					return null;
				case CLI_OPTION_VERSION:
					showVersion(true);
					return null;
				case CLI_OPTION_SHOWDEFAULTS_LONG:
					showDefaults();
					return null;
				case CLI_OPTION_SHOWCLASSPATH:
					setDefaultClasspath(userClasspath);
					showClasspath();
					return null;
				case CLI_OPTION_SHOWCLIARGUMENTS_LONG:
					showCommandLineArguments(args);
					return null;
				case CLI_OPTION_FILE_LONG:
					final String rawFilename = opt.getValue();
					if (rawFilename == null || "".equals(rawFilename)) { //$NON-NLS-1$
						showHelp(true);
					}
					final File file = new File(rawFilename);
					if (!file.canRead()) {
						showError(MessageFormat.format(Messages.Boot_0,
								rawFilename), null);
						return null;
					}
					setPropertiesFrom(file);
					break;
				case CLI_OPTION_CLASSPATH_LONG:
					userClasspath = true;
					addToSystemClasspath(opt.getValue());
					break;
				case CLI_OPTION_OFFLINE_LONG:
					setOffline(true);
					break;
				case CLI_OPTION_RANDOMID_LONG:
					setRandomContextUUID();
					break;
				case CLI_OPTION_BOOTID_LONG:
					setBootAgentTypeContextUUID();
					break;
				case CLI_OPTION_WORLDID_LONG:
					setDefaultContextUUID();
					break;
				case CLI_OPTION_DEFINE_LONG:
					final String name = opt.getValue(0);
					if (!Strings.isNullOrEmpty(name)) {
						setProperty(name, Strings.emptyToNull(opt.getValue(1)));
					}
					break;
				case CLI_OPTION_LOG_LONG:
					verbose = Math.max(LoggerCreator.toInt(opt.getValue()), 0);
					break;
				case CLI_OPTION_QUIET_LONG:
					if (verbose > 0) {
						--verbose;
					}
					break;
				case CLI_OPTION_VERBOSE_LONG:
					++verbose;
					break;
				case CLI_OPTION_NOLOGO_LONG:
					noLogo = true;
					break;
				case CLI_OPTION_EMBEDDED_LONG:
					embedded = true;
					break;
				default:
				}
			}

			setDefaultClasspath(userClasspath);

			// Show the help when there is no argument.
			if (cmd.getArgs().length == 0) {
				showHelp(true);
				return null;
			}

			// Change the verbosity
			setVerboseLevel(verbose);
			// Do nothing at exit
			if (embedded) {
				setExiter(() -> {
					ExecutorService.neverReturn();
				});
			}
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

	/**
	 * Replies the current class path.
	 *
	 * @return the current class path.
	 * @since 0.7
	 */
	public static String getCurrentClasspath() {
		final StringBuilder path = new StringBuilder();
		for (final URL url : getCurrentClassLoader().getURLs()) {
			if (path.length() > 0) {
				path.append(File.pathSeparator);
			}
			final File file = FileSystem.convertURLToFile(url);
			if (file != null) {
				path.append(file.getAbsolutePath());
			}
		}
		return path.toString();
	}

	/**
	 * Replies the current class loader.
	 *
	 * @return the current class loader.
	 * @since 0.7
	 */
	private static URLClassLoader getCurrentClassLoader() {
		synchronized (Boot.class) {
			if (dynamicClassLoader == null) {
				final ClassLoader cl = ClassLoader.getSystemClassLoader();
				if (cl instanceof URLClassLoader) {
					dynamicClassLoader = (URLClassLoader) cl;
				} else {
					dynamicClassLoader = URLClassLoader.newInstance(new URL[0], cl);
				}
			}
			return dynamicClassLoader;
		}
	}

	/** Add the given entries to the system classpath.
	 *
	 * @param entries the new classpath entries. The format of the value is the same as for the <code>-cp</code>
	 *      command-line option of the <code>java</code> tool.
	 */
	@SuppressWarnings("checkstyle:nestedifdepth")
	public static void addToSystemClasspath(String entries) {
		if (!Strings.isNullOrEmpty(entries)) {
			final List<URL> cp = new ArrayList<>();
			final String[] individualEntries = entries.split(Pattern.quote(File.pathSeparator));
			for (final String entry : individualEntries) {
				if (!Strings.isNullOrEmpty(entry)) {
					URL url = FileSystem.convertStringToURL(entry, false);
					if (url != null) {
						// Normalize the folder name in order to have a "/" at the end of the name.
						// Without this "/" the class loader cannot find the resources.
						if (URISchemeType.FILE.isURL(url)) {
							final File file = FileSystem.convertURLToFile(url);
							if (file != null && file.isDirectory()) {
								try {
									url = new URL(URISchemeType.FILE.name(), "", file.getAbsolutePath() + "/"); //$NON-NLS-1$//$NON-NLS-2$
								} catch (MalformedURLException e) {
									//
								}
							}
						}
						cp.add(url);
					}
				}
			}
			final URL[] newcp = new URL[cp.size()];
			cp.toArray(newcp);
			synchronized (Boot.class) {
				dynamicClassLoader = URLClassLoader.newInstance(newcp, ClassLoader.getSystemClassLoader());
			}
		}
	}

	private static void setDefaultClasspath(boolean hasUserClasspath) {
		if (!hasUserClasspath) {
			// Force the current directory to be inside the class path (since 0.7)
			String path = System.getProperty("user.dir"); //$NON-NLS-1$
			if (!path.endsWith(File.separator)) {
				path = path + File.separator;
			}
			addToSystemClasspath(path);
		}
	}

	private static Class<? extends Agent> loadAgentClass(String fullyQualifiedName) {
		final Class<?> type;
		try {
			type = getCurrentClassLoader().loadClass(fullyQualifiedName);
		} catch (Exception e) {
			showError(MessageFormat.format(
					Messages.Boot_1,
					fullyQualifiedName, getCurrentClasspath()),
					e);
			// Even if showError never returns, add the return statement for
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

		showError(MessageFormat.format(Messages.Boot_2,
				fullyQualifiedName), null);
		// Event if showError never returns, add the return statement for
		// avoiding compilation error.
		return null;
	}

	/**
	 * Main function that is parsing the command line and launching the first agent.
	 *
	 * @param args command line arguments
	 * @see #mainWithExitCode(String[])
	 * @see #startJanus(Class, Object...)
	 */
	public static void main(String[] args) {
		mainWithExitCode(args);
	}

	/**
	 * Main function that is parsing the command line and launching the first agent.
	 *
	 * @param args command line arguments
	 * @return the exit code.
	 * @since 0.10
	 * @see #main(String[])
	 * @see #startJanus(Class, Object...)
	 */
	public static int mainWithExitCode(String... args) {
		try {
			Object[] freeArgs = parseCommandLine(args);

			if (JanusConfig.getSystemPropertyAsBoolean(JanusConfig.JANUS_LOGO_SHOW_NAME,
					JanusConfig.JANUS_LOGO_SHOW.booleanValue())) {
				showJanusLogo();
			}

			if (freeArgs.length == 0) {
				showError(Messages.Boot_3, null);
				// Event if showError never returns, add the return statement for
				// avoiding compilation error.
				return ERROR_EXIT_CODE;
			}

			final String agentToLaunch = freeArgs[0].toString();
			freeArgs = Arrays.copyOfRange(freeArgs, 1, freeArgs.length, String[].class);

			// Load the agent class
			final Class<? extends Agent> agent = loadAgentClass(agentToLaunch);
			assert agent != null;

			startJanus(agent, freeArgs);
		} catch (ExitSignal exception) {
			// Be silent
			return exception.getReturnCode();
		} catch (EarlyExitException exception) {
			// Be silent
		} catch (Throwable e) {
			showError(MessageFormat.format(Messages.Boot_4,
					e.getLocalizedMessage()), e);
			// Even if showError never returns, add the return statement for
			// avoiding compilation error.
			return ERROR_EXIT_CODE;
		}
		return SUCCESS_EXIT_CODE;
	}

	/**
	 * Replies the error console stream for logging messages from the boot mechanism.
	 *
	 * <p>The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @return the console logger.
	 * @since 0.10
	 */
	public static PrintStream getErrorConsoleLogger() {
		return errorConsoleLogger == null ? System.err : errorConsoleLogger;
	}

	/**
	 * Replies the standard console stream for logging messages from the boot mechanism.
	 *
	 * <p>The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @return the console logger.
	 * @since 0.10
	 */
	public static PrintStream getStandardConsoleLogger() {
		return standardConsoleLogger == null ? System.out : standardConsoleLogger;
	}

	/**
	 * Change the error console stream for logging messages from the boot mechanism.
	 *
	 * <p>The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @param stream the stream to use for the console logging.
	 * @since 0.10
	 */
	public static void setErrorConsoleLogger(PrintStream stream) {
		errorConsoleLogger = stream;
	}

	/**
	 * Change the error console stream for logging messages from the boot mechanism.
	 *
	 * <p>The console stream is independent of the stream used by the {@link LoggingService logging service} of the platform. Indeed,
	 * the console stream is used for displaying information, warnings and messages before the Janus platform is realy launched.
	 *
	 * @param stream the stream to use for the console logging.
	 * @since 0.10
	 */
	public static void setStandardConsoleLogger(PrintStream stream) {
		standardConsoleLogger = stream;
	}

	/**
	 * Replies the command line options supported by this boot class.
	 *
	 * @return the command line options.
	 */
	public static Options getOptions() {
		final Options options = new Options();

		options.addOption(CLI_OPTION_CLASSPATH_SHORT, CLI_OPTION_CLASSPATH_LONG, true,
				Messages.Boot_24);

		options.addOption(CLI_OPTION_EMBEDDED_SHORT, CLI_OPTION_EMBEDDED_LONG, false,
				Messages.Boot_5);

		options.addOption(CLI_OPTION_BOOTID_SHORT, CLI_OPTION_BOOTID_LONG, false,
				MessageFormat.format(Messages.Boot_6,
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));

		options.addOption(CLI_OPTION_FILE_SHORT, CLI_OPTION_FILE_LONG, true,
				Messages.Boot_7);

		options.addOption(CLI_OPTION_HELP_SHORT, CLI_OPTION_HELP_LONG, false,
				Messages.Boot_8);

		options.addOption(null, CLI_OPTION_NOLOGO_LONG, false,
				Messages.Boot_9);

		options.addOption(CLI_OPTION_OFFLINE_SHORT, CLI_OPTION_OFFLINE_LONG, false,
				MessageFormat.format(Messages.Boot_10, JanusConfig.OFFLINE));

		options.addOption(CLI_OPTION_QUIET_SHORT, CLI_OPTION_QUIET_LONG, false,
				Messages.Boot_11);

		options.addOption(CLI_OPTION_RANDOMID_SHORT, CLI_OPTION_RANDOMID_LONG, false,
				MessageFormat.format(Messages.Boot_12,
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));

		options.addOption(CLI_OPTION_SHOWDEFAULTS_SHORT, CLI_OPTION_SHOWDEFAULTS_LONG, false,
				Messages.Boot_13);

		options.addOption(CLI_OPTION_SHOWCLASSPATH, false,
				Messages.Boot_23);

		options.addOption(null, CLI_OPTION_SHOWCLIARGUMENTS_LONG, false,
				Messages.Boot_14);

		options.addOption(CLI_OPTION_VERBOSE_SHORT, CLI_OPTION_VERBOSE_LONG, false,
				Messages.Boot_15);

		options.addOption(CLI_OPTION_VERSION, false,
				Messages.Boot_25);

		options.addOption(CLI_OPTION_WORLDID_SHORT, CLI_OPTION_WORLDID_LONG, false,
				MessageFormat.format(Messages.Boot_16,
						JanusConfig.BOOT_DEFAULT_CONTEXT_ID_NAME, JanusConfig.RANDOM_DEFAULT_CONTEXT_ID_NAME));
		final StringBuilder b = new StringBuilder();
		int level = 0;
		for (final String logLevel : LoggerCreator.getLevelStrings()) {
			if (b.length() > 0) {
				b.append(", "); //$NON-NLS-1$
			}
			b.append(logLevel);
			b.append(" ("); //$NON-NLS-1$
			b.append(level);
			b.append(")"); //$NON-NLS-1$
			++level;
		}
		Option opt = new Option(CLI_OPTION_LOG_SHORT, CLI_OPTION_LOG_LONG, true,
				MessageFormat.format(Messages.Boot_17,
						JanusConfig.VERBOSE_LEVEL_VALUE, b));
		opt.setArgs(1);
		options.addOption(opt);
		opt = new Option(CLI_OPTION_DEFINE_SHORT, CLI_OPTION_DEFINE_LONG, true,
				Messages.Boot_18);
		opt.setArgs(2);
		opt.setValueSeparator('=');
		opt.setArgName(Messages.Boot_19);
		options.addOption(opt);
		return options;
	}

	/**
	 * Show an error message, and exit.
	 *
	 * <p>This function never returns.
	 *
	 * @param message the description of the error.
	 * @param exception the cause of the error.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static void showError(String message, Throwable exception) {
		PrintWriter logger = new PrintWriter(getErrorConsoleLogger());
		if (message != null && !message.isEmpty()) {
			logger.println(message);
		} else if (exception != null) {
			exception.printStackTrace(logger);
		}
		logger.println();
		logger.flush();

		logger = new PrintWriter(getStandardConsoleLogger());
		showHelp(logger, false);
		showVersion(logger, true);
	}

	/**
	 * Show the help message on the standard console.
	 *
	 * @param exit if {@code true}, this function never returns.
	 */
	public static void showHelp(boolean exit) {
		showHelp(new PrintWriter(getStandardConsoleLogger()), exit);
	}

	private static void showHelp(PrintWriter logger, boolean exit) {
		final HelpFormatter formatter = new HelpFormatter();
		formatter.printHelp(logger, HelpFormatter.DEFAULT_WIDTH,
				getProgramName() + " " //$NON-NLS-1$
				+ Messages.Boot_20,
				"", //$NON-NLS-1$
				getOptions(), HelpFormatter.DEFAULT_LEFT_PAD, HelpFormatter.DEFAULT_DESC_PAD, ""); //$NON-NLS-1$
		logger.flush();
		if (exit) {
			getExiter().exit();
		}
	}

	/** Replies the name of the program.
	 *
	 * @return the name of the program.
	 */
	public static String getProgramName() {
		String programName = JanusConfig.getSystemProperty(JanusConfig.JANUS_PROGRAM_NAME, null);
		if (Strings.isNullOrEmpty(programName)) {
			programName = JanusConfig.JANUS_PROGRAM_NAME_VALUE;
		}
		return programName;
	}

	/**
	 * Show the default values of the system properties. This function never returns.
	 */
	@SuppressWarnings({ "checkstyle:regexp", "resource" })
	public static void showDefaults() {
		final Properties defaultValues = new Properties();
		JanusConfig.getDefaultValues(defaultValues);
		NetworkConfig.getDefaultValues(defaultValues);
		try {
			final OutputStream os = getStandardConsoleLogger();
			defaultValues.storeToXML(os, null);
			os.flush();
		} catch (Throwable e) {
			e.printStackTrace();
		}
		getExiter().exit();
	}

	/**
	 * Show the classpath of the system properties. This function never returns.
	 */
	@SuppressWarnings({ "resource" })
	public static void showClasspath() {
		final String cp = getCurrentClasspath();
		if (!Strings.isNullOrEmpty(cp)) {
			final PrintStream ps = getStandardConsoleLogger();
			for (final String entry : cp.split(Pattern.quote(File.pathSeparator))) {
				ps.println(entry);
			}
			ps.flush();
		}
		getExiter().exit();
	}

	/**
	 * Show the version of Janus.
	 *
	 * @param exit if {@code true}, this function never returns.
	 */
	public static void showVersion(boolean exit) {
		showVersion(new PrintWriter(getStandardConsoleLogger()), exit);
	}

	private static void showVersion(PrintWriter logger, boolean exit) {
		logger.println(MessageFormat.format(Messages.Boot_26, JanusVersion.JANUS_RELEASE_VERSION));
		logger.println(MessageFormat.format(Messages.Boot_27, SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING));
		logger.flush();
		if (exit) {
			getExiter().exit();
		}
	}

	/**
	 * Show the command line arguments. This function never returns.
	 *
	 * @param args the command line arguments.
	 */
	@SuppressWarnings({ "checkstyle:regexp", "resource" })
	public static void showCommandLineArguments(String[] args) {
		try {
			final PrintStream os = getStandardConsoleLogger();
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
		getStandardConsoleLogger().println(Messages.Boot_21);
	}

	/**
	 * Set offline flag of the Janus platform.
	 *
	 * <p>This function is equivalent to the command line option <code>-o</code>.
	 *
	 * <p>This function must be called before launching the Janus platform.
	 *
	 * @param isOffline the offline flag.
	 * @since 2.0.2.0
	 * @see JanusConfig#OFFLINE
	 */
	public static void setOffline(boolean isOffline) {
		System.setProperty(JanusConfig.OFFLINE, Boolean.toString(isOffline));
	}

	/**
	 * Force the Janus platform to use a random identifier for its default context.
	 *
	 * <p>This function is equivalent to the command line option <code>-R</code>.
	 *
	 * <p>This function must be called before launching the Janus platform.
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
	 * <p>This function is equivalent to the command line option <code>-B</code>.
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
	 * <p>This function is equivalent to the command line option <code>-W</code>.
	 *
	 * <p>This function must be called before launching the Janus platform.
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
	 * <p>This function must be called before launching the Janus platform.
	 *
	 * @param level the verbosity level.
	 * @since 2.0.2.0
	 * @see JanusConfig#VERBOSE_LEVEL_NAME
	 */
	public static void setVerboseLevel(int level) {
		System.setProperty(JanusConfig.VERBOSE_LEVEL_NAME, Integer.toString(level));
	}

	/**
	 * Set the system property. This function is an helper for setting a system property usually accessible with {@link System}.
	 *
	 * <p>This function must be called before launching the Janus platform.
	 *
	 * @param name the name of the property.
	 * @param value the value of the property. If the value is <code>null</code> or empty, the property is removed.
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
	 * @param propertyFile the URL from which a stream is opened.
	 * @throws IOException - if the stream cannot be read.
	 * @since 2.0.2.0
	 * @see System#getProperties()
	 * @see Properties#load(InputStream)
	 */
	public static void setPropertiesFrom(URL propertyFile) throws IOException {
		final Properties systemProperties = System.getProperties();
		try (InputStream stream = propertyFile.openStream()) {
			systemProperties.load(stream);
		}
	}

	/**
	 * Set the system property from the content of the file with the given URL. This function is an helper for setting the system
	 * properties usually accessible with {@link System}.
	 *
	 * @param propertyFile the URL from which a stream is opened.
	 * @throws IOException - if the stream cannot be read.
	 * @since 2.0.2.0
	 * @see System#getProperties()
	 * @see Properties#load(InputStream)
	 */
	public static void setPropertiesFrom(File propertyFile) throws IOException {
		final Properties systemProperties = System.getProperties();
		try (InputStream stream = new FileInputStream(propertyFile)) {
			systemProperties.load(stream);
		}
	}

	/**
	 * Replies the identifier of the boot agent from the system's properties. The boot agent is launched with
	 * {@link #startJanus(Class, Object...)}.
	 *
	 * @return the identifier of the boot agent, or <code>null</code> if it is unknown.
	 * @since 2.0.2.0
	 * @see JanusConfig#BOOT_AGENT_ID
	 * @see #startJanus(Class, Object...)
	 */
	public static UUID getBootAgentIdentifier() {
		final String id = JanusConfig.getSystemProperty(JanusConfig.BOOT_AGENT_ID);
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
	 * <p>Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * <p>The platformModule parameter permits to specify the injection module to use. The injection module is in change of
	 * creating/injecting all the components of the platform. The default injection module is retrieved from the system property
	 * with the name stored in {@link JanusConfig#INJECTION_MODULE_NAME}. The default type for the injection module is stored in
	 * the constant {@link JanusConfig#INJECTION_MODULE_NAME_VALUE}.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the launched agent.
	 *
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @see #main(String[])
	 * @see #getBootAgentIdentifier()
	 */
	public static Kernel startJanus(Class<? extends Agent> agentCls, Object... params)
			throws Exception {
		return startJanusWithModuleType(null, agentCls, params);
	}

	/**
	 * Launch the Janus kernel and the first agent in the kernel, and associate a specific UUID to the launched agent.
	 *
	 * <p>Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * @param agentId the identifier of the agent.
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @since 0.10
	 * @see #main(String[])
	 */
	public static Kernel startJanusWithID(UUID agentId, Class<? extends Agent> agentCls, Object... params)
			throws Exception {
		final Class<? extends Module> startupModule = JanusConfig.getSystemPropertyAsClass(Module.class, JanusConfig.INJECTION_MODULE_NAME,
					JanusConfig.INJECTION_MODULE_NAME_VALUE);
		assert startupModule != null : "No platform injection module"; //$NON-NLS-1$
		return internalStartJanus(startupModule.newInstance(), agentId, agentCls, params);
	}

	/**
	 * Launch the Janus kernel and the first agent in the kernel.
	 *
	 * <p>Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * <p>The platformModule parameter permits to specify the injection module to use. The injection module is in change of
	 * creating/injecting all the components of the platform. The default injection module is retrieved from the system property
	 * with the name stored in {@link JanusConfig#INJECTION_MODULE_NAME}. The default type for the injection module is stored in
	 * the constant {@link JanusConfig#INJECTION_MODULE_NAME_VALUE}.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the launched agent.
	 *
	 * @param platformModule type of the injection module to use for initializing the platform, if <code>null</code> the default
	 *        module will be used.
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @since 0.5
	 * @see #main(String[])
	 * @see #getBootAgentIdentifier()
	 */
	public static Kernel startJanusWithModuleType(Class<? extends Module> platformModule, Class<? extends Agent> agentCls, Object... params)
			throws Exception {
		Class<? extends Module> startupModule = platformModule;
		if (startupModule == null) {
			startupModule = JanusConfig.getSystemPropertyAsClass(Module.class, JanusConfig.INJECTION_MODULE_NAME,
					JanusConfig.INJECTION_MODULE_NAME_VALUE);
		}
		assert startupModule != null : "No platform injection module"; //$NON-NLS-1$
		return internalStartJanus(startupModule.newInstance(), null, agentCls, params);
	}

	/**
	 * Launch the Janus kernel and the first agent in the kernel.
	 *
	 * <p>Thus function does not parse the command line. See {@link #main(String[])} for the command line management. When this
	 * function is called, it is assumed that all the system's properties are correctly set.
	 *
	 * <p>The startupModule parameter permits to specify the injection module to use. The injection module is in change of
	 * creating/injecting all the components of the platform. The default injection module is retrieved from the system property
	 * with the name stored in {@link JanusConfig#INJECTION_MODULE_NAME}. The default type for the injection module is stored in
	 * the constant {@link JanusConfig#INJECTION_MODULE_NAME_VALUE}.
	 *
	 * <p>The function {@link #getBootAgentIdentifier()} permits to retrieve the identifier of the launched agent.
	 *
	 * @param startupModule the injection module to use for initializing the platform.
	 * @param agentCls type of the first agent to launch.
	 * @param params parameters to pass to the agent as its initialization parameters.
	 * @return the kernel that was launched.
	 * @throws Exception - if it is impossible to start the platform.
	 * @since 0.5
	 * @see #main(String[])
	 * @see #getBootAgentIdentifier()
	 */
	public static Kernel startJanusWithModule(Module startupModule, Class<? extends Agent> agentCls, Object... params) throws Exception {
		return internalStartJanus(startupModule, null, agentCls, params);
	}

	private static Kernel internalStartJanus(Module startupModule, UUID agentId, Class<? extends Agent> agentCls, Object... params) {
		// Set the boot agent classname
		System.setProperty(JanusConfig.BOOT_AGENT, agentCls.getName());
		// Get the start-up injection module
		final Kernel k = startWithoutAgent(startupModule);
		final Logger logger = k.getLogger();
		if (logger != null) {
			logger.info(MessageFormat.format(Messages.Boot_22, agentCls.getName()));
		}
		final UUID id;
		if (agentId == null) {
			id = k.spawn(agentCls, params);
		} else {
			id = k.spawn(agentId, agentCls, params);
		}
		if (id != null) {
			System.setProperty(JanusConfig.BOOT_AGENT_ID, id.toString());
		} else {
			System.getProperties().remove(JanusConfig.BOOT_AGENT_ID);
		}
		return k;
	}

	/**
	 * Start the SRE without an agent. This function prepare the default context.
	 *
	 * @param startupModule the injection module to use for initializing the platform.
	 * @return the context that is created by the bootstrap. If {@code null} there is no context created.
	 * @since 2.0.7.0
	 */
	public static Kernel startWithoutAgent(Module startupModule) {
		assert startupModule != null : "No platform injection module"; //$NON-NLS-1$
		final Kernel k = Kernel.create(startupModule);
		// Force the bootstrap to reference the created kernel.
		final SREBootstrap bootstrap = SRE.getBootstrap();
		if (bootstrap instanceof Bootstrap) {
			((Bootstrap) bootstrap).setKernel(k);
		}
		return k;
	}

	/**
	 * Start the SRE without an agent. This function prepare the default context.
	 *
	 * @return the context that is created by the bootstrap. If {@code null} there is no context created.
	 * @since 2.0.7.0
	 */
	public static Kernel startWithoutAgent() {
		final Class<? extends Module> startupModule = JanusConfig.getSystemPropertyAsClass(Module.class,
				JanusConfig.INJECTION_MODULE_NAME,
				JanusConfig.INJECTION_MODULE_NAME_VALUE);
		assert startupModule != null : "No platform injection module"; //$NON-NLS-1$
		try {
			return startWithoutAgent(startupModule.newInstance());
		} catch (Exception exception) {
			throw new IllegalStateException(exception);
		}
	}

	/**
	 * Replies the tool for exiting the application.
	 *
	 * @return the tool for exiting the application.
	 */
	public static Exiter getExiter() {
		return applicationExiter == null ? () -> {
			throw new ExitSignal(ERROR_EXIT_CODE);
		} : applicationExiter;
	}

	/**
	 * Changes the tool that permits to stop the application.
	 *
	 * @param exiter the exit tool.
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
	@FunctionalInterface
	public interface Exiter {

		/**
		 * Exit the application.
		 */
		void exit();

	}

	/** Signal for exiting.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	private static final class ExitSignal extends RuntimeException {

		private static final long serialVersionUID = -8424755495462938388L;

		/** Application return code.
		 */
		private final int returnCode;

		/**
		 * Constructor.
		 *
		 * @param returnCode the application return code.
		 */
		ExitSignal(int returnCode) {
			this.returnCode = returnCode;
		}

		/** Replies the application return code.
		 *
		 * @return the code.
		 */
		@Pure
		public int getReturnCode() {
			return this.returnCode;
		}

	}

}
