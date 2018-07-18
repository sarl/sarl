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

package io.sarl.lang.sarlc.configs;

import java.util.logging.Level;
import java.util.logging.Logger;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;
import org.eclipse.xtext.util.Strings;

import io.sarl.maven.bqextension.configs.Config;

/**
 * Configuration for the logging.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@BQConfig("Configuration of the logging service")
public class LoggingConfig implements Config {

	/**
	 * Prefix for the configuration entries of the logging modules.
	 */
	public static final String PREFIX = "log"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the verbosity level of the SRE.
	 *
	 * @see #VERBOSE_LEVEL_VALUE
	 */
	public static final String VERBOSE_LEVEL_NAME = PREFIX + ".level"; //$NON-NLS-1$

	/**
	 * The default verbosity level of the SRE.
	 *
	 * @see #VERBOSE_LEVEL_NAME
	 */
	public static final String VERBOSE_LEVEL_VALUE = "info"; //$NON-NLS-1$

	/**
	 * Name of the property that contains the verbosity level of the SRE.
	 *
	 * @see #VERBOSE_LEVEL_VALUE
	 */
	public static final String DEFAULT_FORMAT_NAME = PREFIX + ".defaultLogFormat"; //$NON-NLS-1$


	/** Name of the property that contains the logging format within the JRE logging system.
	 */
	public static final String JRE_FORMAT_PROPERTY_KEY = "java.util.logging.SimpleFormatter.format"; //$NON-NLS-1$

	/** Default format for the logging messages.
	 *
	 * <p>The parameters for the format string are:
	 * <ul>
	 * <li><code>%1</code>: the date,</li>
	 * <li><code>%2</code>: the name of the calling function,</li>
	 * <li><code>%3</code>: the name of the logger,</li>
	 * <li><code>%4</code>: the logging level,</li>
	 * <li><code>%5</code>: the message, and</li>
	 * <li><code>%6</code>: the throwable.</li>
	 * </ul>
	 */
	public static final String LOG_FORMAT = "[%4$s, %1$tl:%1$tM:%1$tS%1$tp, %3$s] %5$s%6$s%n"; //$NON-NLS-1$

	private String level;

	private Level levelObject;

	private String defaultLogFormat = LOG_FORMAT;

	/** Constructor.
	 */
	public LoggingConfig() {
		setLevel(VERBOSE_LEVEL_VALUE);
	}

	/** Replies the format of a log line.
	 *
	 * <p>The parameters for the format string are:
	 * <ul>
	 * <li><code>%1</code>: the date,</li>
	 * <li><code>%2</code>: the name of the calling function,</li>
	 * <li><code>%3</code>: the name of the logger,</li>
	 * <li><code>%4</code>: the logging level,</li>
	 * <li><code>%5</code>: the message, and</li>
	 * <li><code>%6</code>: the throwable.</li>
	 * </ul>
	 *
	 * @return the log line format.
	 */
	public String getDefaultLogFormat() {
		return this.defaultLogFormat;
	}

	/** Change the format of the log lines. The format must be defined according to the
	 * {@link Logger} API.
	 *
	 * @param format the line format.
	 */
	@BQConfigProperty("Default format of each line within the log")
	public void setDefaultLogFormat(String format) {
		if (Strings.isEmpty(format)) {
			this.defaultLogFormat = LOG_FORMAT;
		} else {
			this.defaultLogFormat = format;
		}
	}

	/**
	 * Replies the verbosity level.
	 *
	 * @param level the verbose level
	 */
	@BQConfigProperty("Name of the default verbosity level")
	public void setLevel(String level) {
		final Level levelObject = parseLoggingLevel(level);
		this.level = levelObject.getName().toLowerCase();
		this.levelObject = levelObject;
	}

	/**
	 * Replies the verbosity level.
	 *
	 * @return the verbose level
	 */
	public String getLevel() {
		if (this.level == null) {
			this.level = VERBOSE_LEVEL_VALUE;
			this.levelObject = null;
		}
		return this.level;
	}

	/**
	 * Replies the verbosity level.
	 *
	 * @return the verbose level
	 */
	public Level getJavaLevelObject() {
		if (this.levelObject == null) {
			this.levelObject = parseLoggingLevel(getLevel());
		}
		return this.levelObject;
	}

	/**
	 * Replies the verbosity level with log4j object.
	 *
	 * @return the verbose level
	 */
	@SuppressWarnings("checkstyle:npathcomplexity")
	public org.apache.log4j.Level getLog4JLevelObject() {
		if (this.levelObject == null) {
			this.levelObject = parseLoggingLevel(getLevel());
		}
		if (this.levelObject == Level.OFF) {
			return org.apache.log4j.Level.OFF;
		}
		if (this.levelObject == Level.SEVERE) {
			return org.apache.log4j.Level.ERROR;
		}
		if (this.levelObject == Level.WARNING) {
			return org.apache.log4j.Level.WARN;
		}
		if (this.levelObject == Level.INFO) {
			return org.apache.log4j.Level.INFO;
		}
		if (this.levelObject == Level.CONFIG || this.levelObject == Level.FINE) {
			return org.apache.log4j.Level.DEBUG;
		}
		if (this.levelObject == Level.FINER || this.levelObject == Level.FINEST) {
			return org.apache.log4j.Level.TRACE;
		}
		if (this.levelObject == Level.ALL) {
			return org.apache.log4j.Level.ALL;
		}
		return org.apache.log4j.Level.INFO;
	}

	/**
	 * Extract the logging level from the given string.
	 *
	 * @param level the string representation of the logging level.
	 * @return the logging level.
	 */
	public static Level parseLoggingLevel(String level) {
		LogLabel defaultLabel = null;
		for (final LogLabel label : LogLabel.values()) {
			if (label.matchesLabel(level)) {
				return label.toLevel();
			}
			if (label.isDefaultLevel()) {
				defaultLabel = label;
			}
		}
		try {
			return fromInt(Integer.parseInt(level));
		} catch (Throwable exception) {
			//
		}
		assert defaultLabel != null;
		return defaultLabel.toLevel();
	}

	/**
	 * Convert a numerical representation of logging level to the logging level.
	 *
	 * @param num the numerical index that corresponds to the given level.
	 * @return the logging level.
	 */
	public static Level fromInt(int num) {
		final LogLabel[] labels = LogLabel.values();
		if (num >= 0 && num < labels.length) {
			return labels[num].toLevel();
		}
		if (num < 0) {
			return Level.OFF;
		}
		return Level.ALL;
	}

	/** Replies all the labels for the logger levels.
	 *
	 * @param separator the separator of the labels.
	 * @return the labels.
	 */
	public static String getLabels(String separator) {
		final StringBuilder all = new StringBuilder();
		for (final LogLabel label : LogLabel.values()) {
			if (all.length() > 0 && separator != null) {
				all.append(separator);
			}
			all.append(label.getLabels()[0]);
		}
		return all.toString();
	}

	/** Replies the configuration factory for the logging.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the logging configuration factory.
	 */
	public static LoggingConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(LoggingConfig.class, PREFIX);
	}

	/** Labels for the logger levels.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public enum LogLabel {

		/** None.
		 */
		OFF {
			@Override
			public String[] getLabels() {
				return new String[] {
					"off", //$NON-NLS-1$
					"none", //$NON-NLS-1$
					"false", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.OFF;
			}
		},
		/** Error.
		 */
		ERROR {
			@Override
			public String[] getLabels() {
				return new String[] {
					"error", //$NON-NLS-1$
					"severe", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.SEVERE;
			}
		},
		/** Warning.
		 */
		WARNING {
			@Override
			public String[] getLabels() {
				return new String[] {
					"warning", //$NON-NLS-1$
					"warn", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.WARNING;
			}
		},
		/** Info.
		 */
		INFO {
			@Override
			public String[] getLabels() {
				return new String[] {
					"info", //$NON-NLS-1$
					"true", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.INFO;
			}
		},
		/** Debug.
		 */
		DEBUG {
			@Override
			public String[] getLabels() {
				return new String[] {
					"debug", //$NON-NLS-1$
					"config", //$NON-NLS-1$
					"fine", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.FINE;
			}
		},
		/** Details.
		 */
		DETAILS {
			@Override
			public String[] getLabels() {
				return new String[] {
					"finer", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.FINER;
			}
		},
		/** More details.
		 */
		MORE_DETAILS {
			@Override
			public String[] getLabels() {
				return new String[] {
					"finest", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.FINEST;
			}
		},
		/** All.
		 */
		ALL {
			@Override
			public String[] getLabels() {
				return new String[] {
					"all", //$NON-NLS-1$
				};
			}

			@Override
			public Level toLevel() {
				return Level.ALL;
			}
		};

		/** Replies the labels.
		 *
		 * @return the labels.
		 */
		public abstract String[] getLabels();

		/** Replies the level for the label.
		 *
		 * @return the level.
		 */
		public abstract Level toLevel();

		/** Replies if the level is the default one.
		 *
		 * @return {@code true} if default level.
		 */
		public boolean isDefaultLevel() {
			return Strings.equal(getLabels()[0], VERBOSE_LEVEL_VALUE);
		}

		/** Replies if the given label matches one of the labels.
		 *
		 * @param label the label to test.
		 * @return {@code true} if the label matches.
		 */
		public boolean matchesLabel(String label) {
			if (label != null) {
				final String lowerLabel = label.toLowerCase();
				for (final String lbl : getLabels()) {
					if (Strings.equal(lowerLabel, lbl)) {
						return true;
					}
				}
			}
			return isDefaultLevel();
		}

	}

}
