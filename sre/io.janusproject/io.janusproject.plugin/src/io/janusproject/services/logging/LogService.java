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
 * <p>
 * The LogService considers the parameters of the functions as:
 * <ul>
 * <li>the messageKey is the name of the message in the property file;</li>
 * <li>the message parameters are the values that will replace the strings {0}, {1}, {2}... in the text extracted from the
 * ressource property;</li>
 * <li>the parameter propertyType is the class from which the filename of the property file will be built.</li>
 * </ul>
 *
 * <p>
 * If a <code>Throwable</code> is passed as parameter, the text of the exception is retreived.
 *
 * <p>
 * If a <code>LogParam</code> is passed as parameter, the <code>toString</code> function will be invoked.
 *
 * <p>
 * For all the other objects, the {@link Object#toString()} function is invoked.
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
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #fineInfo(String, Object...)
	 * @see #finerInfo(String, Object...)
	 */
	void info(String messageKey, Object... message);

	/**
	 * Log an information message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #fineInfo(Class, String, Object...)
	 * @see #finerInfo(Class, String, Object...)
	 */
	void info(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a fine information message.
	 *
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(String, Object...)
	 * @see #finerInfo(String, Object...)
	 */
	void fineInfo(String messageKey, Object... message);

	/**
	 * Log an information message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(Class, String, Object...)
	 * @see #finerInfo(Class, String, Object...)
	 */
	void fineInfo(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a finer information message.
	 *
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(String, Object...)
	 * @see #fineInfo(String, Object...)
	 */
	void finerInfo(String messageKey, Object... message);

	/**
	 * Log a finer information message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 * @see #info(Class, String, Object...)
	 * @see #fineInfo(Class, String, Object...)
	 */
	void finerInfo(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a debug message.
	 *
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void debug(String messageKey, Object... message);

	/**
	 * Log a debug message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void debug(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a warning message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void warning(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a warning message.
	 *
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void warning(String messageKey, Object... message);

	/**
	 * Log an error message.
	 *
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void error(String messageKey, Object... message);

	/**
	 * Log an error message.
	 *
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void error(Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log the given record.
	 *
	 * @param record - the description of the message to log.
	 */
	void log(LogRecord record);

	/**
	 * Log a message.
	 *
	 * @param level - level of logging for the message.
	 * @param propertyType - type that is used to retreive the property file.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void log(Level level, Class<?> propertyType, String messageKey, Object... message);

	/**
	 * Log a warning message.
	 *
	 * @param level - level of logging for the message.
	 * @param messageKey - key of the message in the properties.
	 * @param message - the values to insert into the message in place of the parameter marker (<code>{0}</code>, etc.)
	 */
	void log(Level level, String messageKey, Object... message);

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
