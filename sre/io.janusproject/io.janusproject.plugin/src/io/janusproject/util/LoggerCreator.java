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

package io.janusproject.util;

import java.io.PrintStream;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.logging.SimpleFormatter;
import java.util.logging.StreamHandler;

import io.janusproject.JanusConfig;

/**
 * Helper for creating a logger.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class LoggerCreator {

	private static final String FORMAT_PROPERTY_KEY = "java.util.logging.SimpleFormatter.format"; //$NON-NLS-1$

	/** Default format of a log message within Janus. The parameters for the format string are:
	 * <ul>
	 * <li><code>%1</code>: the date,</li>
	 * <li><code>%2</code>: the name of the calling function,</li>
	 * <li><code>%3</code>: the name of the logger,</li>
	 * <li><code>%4</code>: the logging level,</li>
	 * <li><code>%5</code>: the message, and</li>
	 * <li><code>%6</code>: the throwable.</li>
	 * </ul>
	 */
	private static final String JANUS_FORMAT = "[%4$s, %1$tl:%1$tM:%1$tS%1$tp, %3$s] %5$s%6$s%n"; //$NON-NLS-1$

	private static Level levelFromProperties;

	private LoggerCreator() {
		//
	}

	/**
	 * Change the configuration of the root logger for using the Janus format for the messages.
	 */
	public static void useJanusMessageFormat() {
		final String format = System.getProperty(FORMAT_PROPERTY_KEY, null);
		if (format == null || format.isEmpty()) {
			System.setProperty(FORMAT_PROPERTY_KEY, JANUS_FORMAT);
		}
	}

	/**
	 * Create a logger with the given name for the platform.
	 *
	 * <p>The platform logger has the level {@link Level#ALL}. It override the
	 * output handlers with handlers for the standard output and standard error.
	 * The platform logger is a root logger.
	 *
	 * @return the logger.
	 */
	public static Logger createPlatformLogger() {
		final Logger logger = Logger.getAnonymousLogger();
		for (final Handler handler : logger.getHandlers()) {
			logger.removeHandler(handler);
		}
		final Handler stderr = new StandardErrorOutputConsoleHandler();
		stderr.setLevel(Level.ALL);
		final Handler stdout = new StandardOutputConsoleHandler();
		stdout.setLevel(Level.ALL);
		logger.addHandler(stderr);
		logger.addHandler(stdout);
		logger.setUseParentHandlers(false);
		logger.setLevel(Level.ALL);
		return logger;
	}

	/**
	 * Create a logger with the given name for a module (kernel or agent).
	 *
	 * <p>The level of logging is influence by {@link JanusConfig#VERBOSE_LEVEL_NAME}.
	 *
	 * @param name
	 *            - the name of the new logger.
	 * @param parent
	 *            - the parent logger.
	 * @return the logger.
	 */
	public static Logger createModuleLogger(String name, Logger parent) {
		final Logger logger = Logger.getLogger(name);
		if (parent != null) {
			logger.setParent(parent);
		}
		logger.setUseParentHandlers(true);
		final Level level = getLoggingLevelFromProperties();
		logger.setLevel(level);
		return logger;
	}

	/**
	 * Extract the logging level from the system properties.
	 *
	 * @return the logging level.
	 */
	public static Level getLoggingLevelFromProperties() {
		if (levelFromProperties == null) {
			final String verboseLevel = JanusConfig.getSystemProperty(JanusConfig.VERBOSE_LEVEL_NAME, JanusConfig.VERBOSE_LEVEL_VALUE);
			levelFromProperties = parseLoggingLevel(verboseLevel);
		}
		return levelFromProperties;
	}

	/**
	 * Extract the logging level from the given string.
	 *
	 * @param level
	 *            - the string representation of the logging level.
	 * @return the logging level.
	 */
	@SuppressWarnings({ "checkstyle:returncount", "checkstyle:cyclomaticcomplexity" })
	public static Level parseLoggingLevel(String level) {
		if (level == null) {
			return Level.INFO;
		}
		switch (level.toLowerCase()) {
		case "none": //$NON-NLS-1$
		case "false": //$NON-NLS-1$
		case "0": //$NON-NLS-1$
			return Level.OFF;
		case "severe": //$NON-NLS-1$
		case "error": //$NON-NLS-1$
		case "1": //$NON-NLS-1$
			return Level.SEVERE;
		case "warn": //$NON-NLS-1$
		case "warning": //$NON-NLS-1$
		case "2": //$NON-NLS-1$
			return Level.WARNING;
		case "info": //$NON-NLS-1$
		case "true": //$NON-NLS-1$
		case "3": //$NON-NLS-1$
			return Level.INFO;
		case "fine": //$NON-NLS-1$
		case "config": //$NON-NLS-1$
		case "4": //$NON-NLS-1$
			return Level.FINE;
		case "finer": //$NON-NLS-1$
		case "5": //$NON-NLS-1$
			return Level.FINER;
		case "finest": //$NON-NLS-1$
		case "debug": //$NON-NLS-1$
		case "6": //$NON-NLS-1$
			return Level.FINEST;
		case "all": //$NON-NLS-1$
		case "7": //$NON-NLS-1$
			return Level.ALL;
		default:
			try {
				return fromInt(Integer.parseInt(level));
			} catch (Throwable exception) {
				//
			}
			return Level.INFO;
		}
	}

	/**
	 * Convert a numerical representation of logging level to the logging level.
	 *
	 * @param num
	 *            - the numerical index that corresponds to the given level.
	 * @return the logging level.
	 */
	@SuppressWarnings({ "checkstyle:magicnumber", "checkstyle:returncount" })
	public static Level fromInt(int num) {
		switch (num) {
		case 0:
			return Level.OFF;
		case 1:
			return Level.SEVERE;
		case 2:
			return Level.WARNING;
		case 3:
			return Level.INFO;
		case 4:
			return Level.FINE;
		case 5:
			return Level.FINER;
		case 6:
			return Level.FINEST;
		case 7:
			return Level.ALL;
		default:
			if (num < 0) {
				return Level.OFF;
			}
			return Level.ALL;
		}
	}

	/**
	 * Convert a logging level to its numerical equivalent.
	 *
	 * @param level
	 *            - the logging level.
	 * @return the numerical index that corresponds to the given level.
	 */
	@SuppressWarnings({ "checkstyle:magicnumber", "checkstyle:returncount", "checkstyle:npathcomplexity" })
	public static int toInt(Level level) {
		if (level == Level.OFF) {
			return 0;
		}
		if (level == Level.SEVERE) {
			return 1;
		}
		if (level == Level.WARNING) {
			return 2;
		}
		if (level == Level.INFO) {
			return 3;
		}
		if (level == Level.CONFIG) {
			return 4;
		}
		if (level == Level.FINE) {
			return 4;
		}
		if (level == Level.FINER) {
			return 5;
		}
		if (level == Level.FINEST) {
			return 6;
		}
		if (level == Level.ALL) {
			return 7;
		}
		return 3;
	}

	/**
	 * Convert a string representing a logging level into its numerical representation.
	 *
	 * <p>This is a convinient function that calls {@link #parseLoggingLevel(String)} and {@link #toInt(Level)}.
	 *
	 * @param level
	 *            - the string representation of the logging level.
	 * @return the numerical index that corresponds to the given level.
	 */
	public static int toInt(String level) {
		return toInt(parseLoggingLevel(level));
	}

	/**
	 * Replies the string representations for the logging levels.
	 *
	 * @return the string representations, indexed by the numerical index of the level.
	 */
	public static String[] getLevelStrings() {
		return new String[] {"none", //$NON-NLS-1$
			"error", //$NON-NLS-1$
			"warning", //$NON-NLS-1$
			"info", //$NON-NLS-1$
			"fine", //$NON-NLS-1$
			"finer", //$NON-NLS-1$
			"finest", //$NON-NLS-1$
			"all", //$NON-NLS-1$
		};
	}

	/** A console handler that supports to be link to the standard output or the standard error output.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public abstract static class AbstractStandardConsoleHandler extends StreamHandler {

		/**
		 * Constructor.
		 *
		 * @param stream the original stream.
		 */
		public AbstractStandardConsoleHandler(PrintStream stream) {
			super(stream, new SimpleFormatter());
		}

		@Override
		public synchronized void publish(LogRecord record) {
			super.publish(record);
			flush();
		}

		@Override
		public synchronized void close() {
			flush();
		}

		/** Replies if the given log level is loggable.
		 *
		 * @param recordLevel the level to test.
		 * @return {@code true} if loggable level.
		 */
		protected abstract boolean isLoggable(int recordLevel);

		@Override
		public boolean isLoggable(LogRecord record) {
			if (record != null) {
				final Level level = record.getLevel();
				assert level != null;
				if (isLoggable(level.intValue())) {
					return super.isLoggable(record);
				}
			}
			return false;
		}

	}

	/** A console handler that supports to be link to the standard output.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class StandardOutputConsoleHandler extends AbstractStandardConsoleHandler {

		/** Constructor.
		 */
		public StandardOutputConsoleHandler() {
			super(System.out);
		}

		@Override
		public boolean isLoggable(int level) {
			return level < Level.WARNING.intValue();
		}

	}

	/** A console handler that supports to be link to the standard error output.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.6
	 */
	public static class StandardErrorOutputConsoleHandler extends AbstractStandardConsoleHandler {

		/** Constructor.
		 */
		public StandardErrorOutputConsoleHandler() {
			super(System.err);
		}

		@Override
		public boolean isLoggable(int level) {
			return level >= Level.WARNING.intValue();
		}

	}

}
