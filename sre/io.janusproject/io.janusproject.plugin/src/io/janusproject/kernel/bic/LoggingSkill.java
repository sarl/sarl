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

package io.janusproject.kernel.bic;

import java.text.MessageFormat;
import java.util.UUID;
import java.util.function.Supplier;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.base.Throwables;
import com.google.inject.Inject;

import io.janusproject.services.logging.LogService;
import io.janusproject.util.LoggerCreator;

import io.sarl.core.Logging;
import io.sarl.lang.core.Agent;

/**
 * Janus implementation of SARL's {@link Logging} built-in capacity.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class LoggingSkill extends BuiltinSkill implements Logging {

	private static final Level DEBUG_LEVEL = Level.FINE;

	private static final Level INFO_LEVEL = Level.INFO;

	private static final Level WARNING_LEVEL = Level.WARNING;

	private static final Level ERROR_LEVEL = Level.SEVERE;

	private static int installationOrder = -1;

	private LogService logService;

	private Logger logger;

	/** Constructor.
	 * @param agent owner of this skill.
	 */
	LoggingSkill(Agent agent) {
		super(agent);
	}

	/** {@inheritDoc}
	 *
	 * @deprecated since 0.10
	 */
	@Override
	@Deprecated
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	public Logger getLogger() {
		return this.logger;
	}

	/** Change the reference to the logging service.
	 *
	 * @param service the service.
	 */
	@Inject
	public void setLoggingService(LogService service) {
		assert service != null;
		this.logService = service;
	}

	/** Change the internal logger.
	 *
	 * @param logger the logger.
	 */
	public void setLogger(Logger logger) {
		assert logger != null;
		this.logger = logger;
	}

	@Override
	protected void install() {
		final UUID agentId = getOwner().getID();
		final String loggerName = MessageFormat.format(Messages.LoggingSkill_0, agentId);
		final Logger logger = this.logService.createAgentLogger(loggerName);
		setLogger(logger);
	}

	@Override
	public void setLoggingName(String name) {
		String loggerName = name;
		if (loggerName == null || loggerName.isEmpty()) {
			loggerName = MessageFormat.format(Messages.LoggingSkill_0, getOwner().getID());
		}
		Logger logger = getLogger();
		final Level level = logger != null ? logger.getLevel() : null;
		logger = this.logService.createAgentLogger(loggerName, level);
		setLogger(logger);
	}

	@Override
	public void println(Object message) {
		info(message);
	}

	@Override
	public void error(Object message, Throwable exception, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(ERROR_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			if (exception != null) {
				final LogRecord lr = new LogRecord(ERROR_LEVEL, loggeableMessage);
		        lr.setParameters(parameters);
		        lr.setThrown(Throwables.getRootCause(exception));
				logger.log(lr);
			} else {
				logger.log(ERROR_LEVEL, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void error(Object message, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(ERROR_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			logger.log(ERROR_LEVEL, loggeableMessage, parameters);
		}
	}

	@Override
	public void error(Supplier<String> messageProvider) {
		final Logger logger = getLogger();
		if (logger.isLoggable(ERROR_LEVEL) && messageProvider != null) {
			logger.log(ERROR_LEVEL, messageProvider);
		}
	}

	@Override
	public void warning(Object message, Throwable exception, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(WARNING_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				if (exception != null) {
					final LogRecord lr = new LogRecord(WARNING_LEVEL, loggeableMessage);
			        lr.setParameters(parameters);
			        lr.setThrown(exception);
					logger.log(lr);
				} else {
					logger.log(WARNING_LEVEL, loggeableMessage, parameters);
				}
			}
		}
	}

	@Override
	public void warning(Object message, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(WARNING_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				logger.log(WARNING_LEVEL, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void warning(Supplier<String> messageProvider) {
		final Logger logger = getLogger();
		if (logger.isLoggable(WARNING_LEVEL) && messageProvider != null) {
			logger.log(WARNING_LEVEL, messageProvider);
		}
	}

	@Override
	public void info(Object message, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(INFO_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				logger.log(INFO_LEVEL, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void info(Supplier<String> messageProvider) {
		final Logger logger = getLogger();
		if (logger.isLoggable(INFO_LEVEL) && messageProvider != null) {
			logger.log(INFO_LEVEL, messageProvider);
		}
	}

	@Override
	public void debug(Object message, Object... parameters) {
		final Logger logger = getLogger();
		if (logger.isLoggable(DEBUG_LEVEL) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				logger.log(DEBUG_LEVEL, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void debug(Supplier<String> messageProvider) {
		final Logger logger = getLogger();
		if (logger.isLoggable(DEBUG_LEVEL) && messageProvider != null) {
			logger.log(DEBUG_LEVEL, messageProvider);
		}
	}

	@Override
	public boolean isErrorLogEnabled() {
		return getLogger().isLoggable(ERROR_LEVEL);
	}

	@Override
	public boolean isWarningLogEnabled() {
		return getLogger().isLoggable(WARNING_LEVEL);
	}

	@Override
	public boolean isInfoLogEnabled() {
		return getLogger().isLoggable(INFO_LEVEL);
	}

	@Override
	public boolean isDebugLogEnabled() {
		return getLogger().isLoggable(DEBUG_LEVEL);
	}

	@Override
	public int getLogLevel() {
		return LoggerCreator.toInt(getLogger().getLevel());
	}

	@Override
	public void setLogLevel(int level) {
		final Level lvl = LoggerCreator.fromInt(level);
		getLogger().setLevel(lvl);
	}

}
