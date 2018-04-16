/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2018 the original authors or authors.
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

import java.io.PrintWriter;
import java.text.MessageFormat;
import java.util.Iterator;

import com.google.inject.Injector;
import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;
import org.apache.log4j.ConsoleAppender;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;
import org.apache.log4j.PatternLayout;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.util.Strings;

import io.sarl.lang.SARLStandaloneSetup;
import io.sarl.lang.SARLVersion;

/** Main entry point for the SARL batch compiler.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
public final class Main {

	/** A conversion pattern for the logger.
	 */
	public static final String LOGGER_PATTERN = "%-5p %m%n"; //$NON-NLS-1$

	private static final String CLI_OPTION_OUTPUT_DIRECTORY_SHORT = "d"; //$NON-NLS-1$

	private static final String CLI_OPTION_OUTPUT_DIRECTORY_LONG = "dir"; //$NON-NLS-1$

	private static final String CLI_OPTION_TEMP_DIRECTORY_SHORT = "td"; //$NON-NLS-1$

	private static final String CLI_OPTION_TEMP_DIRECTORY_LONG = "tempdir"; //$NON-NLS-1$

	private static final String CLI_OPTION_CLASSPATH_SHORT = "cp"; //$NON-NLS-1$

	private static final String CLI_OPTION_CLASSPATH_LONG = "classpath"; //$NON-NLS-1$

	private static final String CLI_OPTION_BOOTCLASSPATH_SHORT = "bp"; //$NON-NLS-1$

	private static final String CLI_OPTION_BOOTCLASSPATH_LONG = "bootclasspath"; //$NON-NLS-1$

	private static final String CLI_OPTION_ENCODING_SHORT = "e"; //$NON-NLS-1$

	private static final String CLI_OPTION_ENCODING_LONG = "encoding"; //$NON-NLS-1$

	private static final String CLI_OPTION_JAVA_SOURCE_VERSION_SHORT = "jsv"; //$NON-NLS-1$

	private static final String CLI_OPTION_JAVA_SOURCE_VERSION_LONG = "javaSourceVersion"; //$NON-NLS-1$

	private static final String CLI_OPTION_VERBOSE_SHORT = "v"; //$NON-NLS-1$

	private static final String CLI_OPTION_VERBOSE_LONG = "verbose"; //$NON-NLS-1$

	private static final String CLI_OPTION_QUIET_SHORT = "q"; //$NON-NLS-1$

	private static final String CLI_OPTION_QUIET_LONG = "quiet"; //$NON-NLS-1$

	private static final String CLI_OPTION_DEBUG_SHORT = "X"; //$NON-NLS-1$

	private static final String CLI_OPTION_DEBUG_LONG = "debug"; //$NON-NLS-1$

	private static final String CLI_OPTION_VERSION = "version"; //$NON-NLS-1$

	private static final String CLI_OPTION_HELP = "help"; //$NON-NLS-1$

	private static final String CLI_OPTION_JAVA_COMPILER_SHORT = "jc"; //$NON-NLS-1$

	private static final String CLI_OPTION_JAVA_COMPILER_LONG = "javac"; //$NON-NLS-1$

	private static final String CLI_OPTION_WRITE_TRACES = "writetraces"; //$NON-NLS-1$

	private static final String CLI_OPTION_WRITE_STORAGES = "writestorages"; //$NON-NLS-1$

	private static final String CLI_OPTION_GENERATE_INLINES = "inlines"; //$NON-NLS-1$

	private static final String CLI_OPTION_GENERATE_PURES = "pures"; //$NON-NLS-1$

	private static final String CLI_OPTION_GENERATE_EQUALITY_TEST_FUNCTIONS = "equalsFunctions"; //$NON-NLS-1$

	private static final String CLI_OPTION_NOWARNING = "nowarn"; //$NON-NLS-1$

	private static final String CLI_OPTION_WARNINGISERROR = "werror"; //$NON-NLS-1$

	private static final String CLI_OPTION_WARNING_LEVEL_SHORT = "w"; //$NON-NLS-1$

	private static final String CLI_OPTION_WARNING_LEVEL_LONG = "warn"; //$NON-NLS-1$

	private static final int SUCCESS_CODE = 0;

	private static final int ERROR_CODE = 255;

	private static final String SARL_COMPILER_NAME = "sarlc"; //$NON-NLS-1$

	private static final String COMPILER_PROGRAMNAME_PROPERTY_NAME = "sarlc.programName"; //$NON-NLS-1$

	private Main() {
		//
	}

	/** Main programm of the batch compiler.
	 *
	 * <p>This function never returns. It invokes {@link #runCompiler(String...)}
	 * and stop the JVM with the replied exist code.
	 *
	 * @param args the command line arguments.
	 * @see #runCompiler(String...)
	 */
	public static void main(String[] args) {
		final int retCode = runCompiler(args);
		System.exit(retCode);
	}

	/** Run the batch compiler.
	 *
	 * <p>This function runs the compiler and exits with the return code.
	 *
	 * @param args the command line arguments.
	 * @return the exit code.
	 * @see #main(String[])
	 */
	public static int runCompiler(String... args) {
		configureLogger();
		final Injector injector = SARLStandaloneSetup.doSetup();
		assert injector != null;
		final SarlBatchCompiler compiler = injector.getInstance(SarlBatchCompiler.class);
		assert compiler != null;
		parseCommandLine(args, compiler);
		if (!compiler.compile()) {
			return ERROR_CODE;
		}
		return SUCCESS_CODE;
	}

	private static void configureLogger() {
		final Logger root = Logger.getRootLogger();
		root.removeAllAppenders();
		root.addAppender(new ConsoleAppender(
				new PatternLayout(LOGGER_PATTERN)));
	}

	private static CommandLineParser createCommandLineParser() {
		return new DefaultParser();
	}

	/**
	 * Parse the command line.
	 *
	 * @param args the CLI arguments given to the program.
	 * @param compiler the compiler to configure.
	 */
	@SuppressWarnings({"checkstyle:cyclomaticcomplexity", "checkstyle:npathcomplexity"})
	public static void parseCommandLine(String[] args, SarlBatchCompiler compiler) {
		final CommandLineParser parser = createCommandLineParser();
		try {
			final CommandLine cmd = parser.parse(getOptions(), args);

			final Iterator<Option> optIterator = cmd.iterator();
			while (optIterator.hasNext()) {
				final Option opt = optIterator.next();
				String optLabel = opt.getLongOpt();
				if (optLabel == null) {
					optLabel = opt.getOpt();
				}
				final String strvalue;
				switch (optLabel) {
				case CLI_OPTION_HELP:
					printUsage();
					return;
				case CLI_OPTION_VERSION:
					printVersion();
					return;
				case CLI_OPTION_VERBOSE_LONG:
					compiler.getLogger().setLevel(Level.toLevel(
							compiler.getLogger().getLevel().toInt() + 1));
					break;
				case CLI_OPTION_QUIET_LONG:
					compiler.getLogger().setLevel(Level.ERROR);
					break;
				case CLI_OPTION_DEBUG_LONG:
					compiler.setJavaCompilerVerbose(true);
					compiler.getLogger().setLevel(Level.DEBUG);
					break;
				case CLI_OPTION_OUTPUT_DIRECTORY_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setOutputPath(strvalue);
					break;
				case CLI_OPTION_TEMP_DIRECTORY_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setTempDirectory(strvalue);
					break;
				case CLI_OPTION_CLASSPATH_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setClassPath(strvalue);
					break;
				case CLI_OPTION_BOOTCLASSPATH_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setBootClassPath(strvalue);
					break;
				case CLI_OPTION_ENCODING_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setFileEncoding(strvalue);
					break;
				case CLI_OPTION_JAVA_SOURCE_VERSION_LONG:
					strvalue = getStringValue(opt);
					if (strvalue == null) {
						printUsage();
						return;
					}
					compiler.setJavaSourceVersion(strvalue);
					break;
				case CLI_OPTION_JAVA_COMPILER_LONG:
					compiler.setJavaPostCompilationEnable(getBooleanValue(opt));
					break;
				case CLI_OPTION_WRITE_TRACES:
					compiler.setWriteTraceFiles(getBooleanValue(opt));
					break;
				case CLI_OPTION_WRITE_STORAGES:
					compiler.setWriteStorageFiles(getBooleanValue(opt));
					break;
				case CLI_OPTION_GENERATE_INLINES:
					compiler.setGenerateInlineAnnotation(getBooleanValue(opt));
					break;
				case CLI_OPTION_GENERATE_PURES:
					compiler.setGeneratePureAnnotation(getBooleanValue(opt));
					break;
				case CLI_OPTION_GENERATE_EQUALITY_TEST_FUNCTIONS:
					compiler.setGenerateEqualityTestFunctions(getBooleanValue(opt));
					break;
				case CLI_OPTION_NOWARNING:
					compiler.setAllWarningSeverities(Severity.IGNORE);
					break;
				case CLI_OPTION_WARNINGISERROR:
					compiler.setAllWarningSeverities(Severity.ERROR);
					break;
				case CLI_OPTION_WARNING_LEVEL_LONG:
					compiler.setWarningSeverity(opt.getValue(0),
							parseWarningSeverity(opt.getValue(1)));
					break;
				default:
				}
			}

			// Show the help when there is no argument.
			if (cmd.getArgs().length == 0) {
				printUsage();
				return;
			}

			for (final String cliArg : cmd.getArgs()) {
				compiler.addSourcePath(cliArg);
			}
		} catch (ParseException e) {
			showError(e);
		}
	}

	private static Severity parseWarningSeverity(String code) {
		if (!Strings.isEmpty(code)) {
			final String lccode = code.toLowerCase();
			switch (lccode) {
			case "err": //$NON-NLS-1$
			case "error": //$NON-NLS-1$
				return Severity.ERROR;
			case "warn": //$NON-NLS-1$
			case "warning": //$NON-NLS-1$
				return Severity.WARNING;
			case "info": //$NON-NLS-1$
			case "information": //$NON-NLS-1$
				return Severity.INFO;
			case "ign": //$NON-NLS-1$
			case "ignore": //$NON-NLS-1$
			case "none": //$NON-NLS-1$
				return Severity.IGNORE;
			default:
			}
		}
		return null;
	}

	private static boolean getBooleanValue(Option option) {
		final String value = option.getValue();
		if (value != null && Boolean.parseBoolean(value)) {
			return true;
		}
		return false;
	}

	private static String getStringValue(Option option) {
		final String value = option.getValue();
		if (value == null || "".equals(value)) { //$NON-NLS-1$
			return null;
		}
		return value;
	}

	/**
	 * Replies the command line options supported by this boot class.
	 *
	 * @return the command line options.
	 */
	public static Options getOptions() {
		final Options options = new Options();
		options.addOption(CLI_OPTION_OUTPUT_DIRECTORY_SHORT, CLI_OPTION_OUTPUT_DIRECTORY_LONG, true,
				Messages.Main_1);
		options.addOption(CLI_OPTION_TEMP_DIRECTORY_SHORT, CLI_OPTION_TEMP_DIRECTORY_LONG, true,
				Messages.Main_2);
		options.addOption(CLI_OPTION_CLASSPATH_SHORT, CLI_OPTION_CLASSPATH_LONG, true,
				Messages.Main_3);
		options.addOption(CLI_OPTION_BOOTCLASSPATH_SHORT, CLI_OPTION_BOOTCLASSPATH_LONG, true,
				Messages.Main_4);
		options.addOption(CLI_OPTION_ENCODING_SHORT, CLI_OPTION_ENCODING_LONG, true,
				Messages.Main_5);
		options.addOption(CLI_OPTION_JAVA_SOURCE_VERSION_SHORT, CLI_OPTION_JAVA_SOURCE_VERSION_LONG, true,
				Messages.Main_6);
		options.addOption(CLI_OPTION_JAVA_COMPILER_SHORT, CLI_OPTION_JAVA_COMPILER_LONG, true,
				Messages.Main_0);
		options.addOption(CLI_OPTION_WRITE_TRACES, true,
				Messages.Main_14);
		options.addOption(CLI_OPTION_WRITE_STORAGES, true,
				Messages.Main_15);
		options.addOption(CLI_OPTION_GENERATE_INLINES, true,
				Messages.Main_16);
		options.addOption(CLI_OPTION_VERBOSE_SHORT, CLI_OPTION_VERBOSE_LONG, false,
				Messages.Main_7);
		options.addOption(CLI_OPTION_QUIET_SHORT, CLI_OPTION_QUIET_LONG, false,
				Messages.Main_18);
		options.addOption(CLI_OPTION_DEBUG_SHORT, CLI_OPTION_DEBUG_LONG, false,
				Messages.Main_17);
		options.addOption(CLI_OPTION_VERSION, false,
				Messages.Main_8);
		options.addOption(CLI_OPTION_HELP, false,
				Messages.Main_9);
		options.addOption(CLI_OPTION_NOWARNING, false,
				Messages.Main_19);
		options.addOption(CLI_OPTION_WARNINGISERROR, false,
				Messages.Main_20);
		final Option wloption = new Option(CLI_OPTION_WARNING_LEVEL_SHORT, CLI_OPTION_WARNING_LEVEL_LONG, true,
				Messages.Main_21);
		wloption.setArgs(2);
		wloption.setOptionalArg(false);
		wloption.setValueSeparator('=');
		options.addOption(wloption);
		return options;
	}

	/**
	 * Show an error message, and exit.
	 *
	 * <p>This function never returns.
	 *
	 * @param exception the cause of the error.
	 */
	@SuppressWarnings("checkstyle:regexp")
	protected static void showError(Throwable exception) {
		Throwable ex = exception;
		while (ex != null && ex.getCause() != null && ex.getCause() != ex) {
			ex = ex.getCause();
		}
		if (ex == null) {
			ex = exception;
		}
		try (PrintWriter logger = new PrintWriter(System.err)) {
			final String message = ex.getLocalizedMessage();
			if (message != null && !message.isEmpty()) {
				logger.println(message);
			} else {
				logger.println(ex.getClass().getName());
				ex.printStackTrace(logger);
			}
			logger.flush();
		}
		System.exit(ERROR_CODE);
	}

	/** Replies the name of the SARL compiler command-line program.
	 *
	 * @return the name of the CLI compiler for SARL.
	 */
	public static String getCompilerProgramName() {
		String programName = System.getProperty(COMPILER_PROGRAMNAME_PROPERTY_NAME, null);
		if (Strings.isEmpty(programName)) {
			programName = SARL_COMPILER_NAME;
		}
		return programName;
	}

	private static void printUsage() {
		try (PrintWriter stream = new PrintWriter(System.err)) {
			final HelpFormatter formatter = new HelpFormatter();
			formatter.printHelp(stream, HelpFormatter.DEFAULT_WIDTH,
					MessageFormat.format(Messages.Main_10, getCompilerProgramName(), CLI_OPTION_OUTPUT_DIRECTORY_LONG),
					"", //$NON-NLS-1$
					getOptions(), HelpFormatter.DEFAULT_LEFT_PAD, HelpFormatter.DEFAULT_DESC_PAD, ""); //$NON-NLS-1$
			stream.flush();
		}
		System.exit(ERROR_CODE);
	}

	private static void printVersion() {
		try (PrintWriter stream = new PrintWriter(System.out)) {
			stream.println(Messages.Main_11 + SARLVersion.SARL_RELEASE_VERSION);
			stream.println(Messages.Main_12 + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING);
			stream.println(Messages.Main_13
					+ System.getProperty("java.version")); //$NON-NLS-1$
			stream.flush();
		}
		System.exit(SUCCESS_CODE);
	}

}
