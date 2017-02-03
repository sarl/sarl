/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.janusproject.services.logging;

import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import io.janusproject.services.DependentService;

/**
 * This class enables to log information by ensuring that the values of the parameters are not evaluated until the information
 * should be really log, according to the log level.
 *
 * <p>The LogService considers the parameters of the functions as:
 * <ul>
 * <li>the messageKey is the name of the message in the property file;</li>
 * <li>the message parameters are the values that will replace the strings {0}, {1}, {2}... in the text extracted from the
 * ressource property;</li>
 * <li>the parameter propertyType is the class from which the filename of the property file will be built.</li>
 * </ul>
 *
 * <p>If a <code>Throwable</code> is passed as parameter, the text of the exception is retreived.
 *
 * <p>If a <code>LogParam</code> is passed as parameter, the <code>toString</code> function will be invoked.
 *
 * <p>For all the other objects, the {@link Object#toString()} function is invoked.
 *
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface LogService extends DependentService {

	/**
	 * Log an information message.
	 *
	 * @param message - the message.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #fineInfo(String, Object...)
	 * @see #finerInfo(String, Object...)
	 */
	void info(String message, Object... params);

	/**
	 * Log a fine information message.
	 *
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(String, Object...)
	 * @see #finerInfo(String, Object...)
	 */
	void fineInfo(String message, Object... params);

	/**
	 * Log a finer information message.
	 *
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(String, Object...)
	 * @see #fineInfo(String, Object...)
	 */
	void finerInfo(String message, Object... params);

	/**
	 * Log a debug message.
	 *
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void debug(String message, Object... params);

	/**
	 * Log a warning message.
	 *
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void warning(String message, Object... params);

	/**
	 * Log a warning message.
	 *
	 * @param exception - the exception to log.
	 * @since 0.5
	 */
	void warning(Throwable exception);

	/**
	 * Log an error message.
	 *
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void error(String message, Object... params);

	/**
	 * Log an error message.
	 *
	 * @param exception - the exception to log.
	 * @since 0.5
	 */
	void error(Throwable exception);

	/**
	 * Log the given record.
	 *
	 * @param record - the description of the message to log.
	 */
	void log(LogRecord record);

	/**
	 * Log a warning message.
	 *
	 * @param level - level of logging for the message.
	 * @param message - the message in the properties.
	 * @param params - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void log(Level level, String message, Object... params);

	/**
	 * Replies the logger.
	 *
	 * @return the logger.
	 */
	Logger getLogger();

	/**
	 * Change the logger.
	 *
	 * @param logger - the background logger instance.
	 */
	void setLogger(Logger logger);

	/**
	 * Change the filter that permits to output particular logs.
	 *
	 * @param filter - the filter.
	 */
	void setFilter(Filter filter);

	/**
	 * Replies the filter that permits to output particular logs.
	 *
	 * @return the filter
	 */
	Filter getFilter();

	/**
	 * Check if a message of the given level would actually be logged by this logger. This check is based on the Loggers effective
	 * level, which may be inherited from its parent.
	 *
	 * @param level - a message logging level
	 * @return <code>true</code> if the given message level is currently being logged.
	 */
	boolean isLoggeable(Level level);

	/**
	 * Replies the level of logging.
	 *
	 * @return the level of logging.
	 */
	Level getLevel();

	/**
	 * Change the level of logging.
	 *
	 * @param level - the level of the logging.
	 */
	void setLevel(Level level);

	/**
	 * Utility to put objec that us asynchronously evaluated by the {@link LogService}.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see LogService
	 */
	public interface LogParam {

		@Override
		String toString();

	}

}
