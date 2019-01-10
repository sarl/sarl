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

package io.janusproject.services.logging;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import io.janusproject.services.DependentService;
import io.janusproject.util.LoggerCreator;

/**
 * This class enables to log information by ensuring that the values of the parameters are not evaluated until the information
 * should be really log, according to the log level.
 *
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface LogService extends DependentService {

	/** Create a logger for an agent.
	 *
	 * @param loggingName the name to associate to the logger, usually the identifier of the agent.
	 * @param initialLevel the initial logging level.
	 * @return the agent logger.
	 * @since 0.7
	 */
	default Logger createAgentLogger(String loggingName, Level initialLevel) {
		final Logger logger = LoggerCreator.createModuleLogger(loggingName, getPlatformLogger());
		if (initialLevel != null) {
			logger.setLevel(initialLevel);
		}
		return logger;
	}

	/** Create a logger for an agent.
	 *
	 * @param loggingName the name to associate to the logger, usually the identifier of the agent.
	 * @return the agent logger.
	 * @since 0.7
	 */
	default Logger createAgentLogger(String loggingName) {
		return createAgentLogger(loggingName, null);
	}

	/**
	 * Replies the logger of the entire platform. The platform logger is the
	 * parent logger for the ones associated to the platform kernel and the agents.
	 *
	 * <p>Usually, the platform kernel does not filter the messages according to the level.
	 *
	 * @return the logger.
	 * @since 0.7
	 * @see #getKernelLogger()
	 */
	Logger getPlatformLogger();

	/**
	 * Replies the logger of the kernel.
	 *
	 * @return the logger.
	 * @since 0.7
	 * @see #getPlatformLogger()
	 */
	Logger getKernelLogger();

	/**
	 * Ensure that the log record has the minimum set of information for a useful logging message.
	 *
	 * @param record the record to prepare.
	 * @param loggerName the name of the logger.
	 * @param exception the cause of the log.
	 * @return the {@code record}.
	 * @since 0.9
	 */
	LogRecord prepareLogRecord(LogRecord record, String loggerName, Throwable exception);

}
