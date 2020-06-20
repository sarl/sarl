/* 
 * $Id$
 * 
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 * 
 * Copyright (C) 2014-2020 the original authors or authors.
 * 
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * 
 * http://www.apache.org/licenses/LICENSE-2.0
 * 
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.maven.bootiqueapp.config;

import java.util.logging.Level;
import java.util.logging.Logger;

import io.bootique.annotation.BQConfig;
import io.bootique.annotation.BQConfigProperty;
import io.bootique.config.ConfigurationFactory;
import org.eclipse.xtext.util.Strings;

/** 
 * Configuration for the loggers.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.12
 */
@BQConfig("Configuration of the loggers")
public class LogConfig {

	/** 
	 * Prefix for the configuration entries of the logger modules.
	 */
	public static final String PREFIX = "log";

	/**
	 * Name of the property that contains the logging level.
	 */
	public static final String  LEVEL = PREFIX + ".level";

	/** 
	 * Default value for the property that contains the logging level.
	 */
	public static final Level DEFAULT_LEVEL = Level.INFO;

	/**
	 * Name of the property that contains the log format.
	 */
	public static final String LOG_FORMAT = PREFIX + ".logFormat"; //$NON-NLS-1$

	/** Default conversion pattern for the logger.
	 */
	public static final String DEFAULT_LOG_FORMAT = "%-5p %m%n"; //$NON-NLS-1$

	private LogLevel level;

	private String logFormat;

	/** Replies the configuration factory for the logging.
	 *
	 * @param configFactory the general configuration factory.
	 * @return the logging configuration factory.
	 */
	public static LogConfig getConfiguration(ConfigurationFactory configFactory) {
		assert configFactory != null;
		return configFactory.config(LogConfig.class, PREFIX);
	}

	/** Replies the level.
	 *
	 * @return the level.
	 */
	public LogLevel getLevel() {
		if (this.level == null) {
			this.level = new LogLevel(DEFAULT_LEVEL);
		}
		return this.level;
	}

	/** Change the level.
	 *
	 * @param level the level.
	 */
	@BQConfigProperty("Logging level of a given logger and its children.")
	public void setLevel(LogLevel level) {
		this.level = level;
	}

	/** Configure the given logger from the configuration.
	 *
	 * @param logger the logger to configure.
	 * @return the logger.
	 */
	public Logger configureLogger(Logger logger) {
		final String format = getLogFormat();
		if (!Strings.isEmpty(format)) {
			/*final Enumeration<? extends Appender> allAppenders  = logger.getAllAppenders();
			while (allAppenders.hasMoreElements()) {
				final Appender appender = allAppenders.nextElement();
				appender.setLayout(new PatternLayout(format));
			}*/
		}
		logger.setLevel(getLevel().getJul());
		return logger;
	}

	/** Configure the given logger from the configuration.
	 *
	 * @param loggerName the name of the logger to configure.
	 * @return the logger.
	 */
	public Logger configureLogger(String loggerName) {
		return configureLogger(Logger.getLogger(loggerName));
	}

	/** Replies the format of the log.
	 *
	 * @return the format, never {@code null}.
	 */
	public String getLogFormat() {
		if (this.logFormat == null) {
			this.logFormat = DEFAULT_LOG_FORMAT;
		}
		return this.logFormat;
	}

	/** Change the format of the log.
	 *
	 * @param format the format.
	 */
	@BQConfigProperty("Log format specification used by child appenders unless redefined at the appender level, or not relevant for a given type of appender. The spec is compatible with Log4j framework. Default format is '" + DEFAULT_LOG_FORMAT + "'.")
	public void setLogFormat(String format) {
		this.logFormat = format;
	}

}
