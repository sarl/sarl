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

	private static int installationOrder = -1;

	@Inject
	private LogService logService;

	private Logger logger;

	/** Constructor.
	 * @param agent owner of this skill.
	 */
	LoggingSkill(Agent agent) {
		super(agent);
	}

	@Override
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

	@Override
	protected void install() {
		final UUID agentId = getOwner().getID();
		final String loggerName = MessageFormat.format(Messages.LoggingSkill_0, agentId);
		this.logger = LoggerCreator.createLogger(loggerName, this.logService.getLogger());
	}

	@Override
	public void setLoggingName(String name) {
		String loggingName = name;
		if (loggingName == null || loggingName.isEmpty()) {
			loggingName = MessageFormat.format(Messages.LoggingSkill_0, getOwner().getID());
		}
		final Level level = this.logger.getLevel();
		this.logger = LoggerCreator.createLogger(loggingName, this.logService.getLogger());
		this.logger.setLevel(level);
	}

	@Override
	public void println(Object message) {
		info(message);
	}

	@Override
	public void error(Object message, Throwable exception, Object... parameters) {
		if (this.logger.isLoggable(Level.SEVERE) && message != null) {
			final String loggeableMessage = message.toString();
			if (exception != null) {
				final LogRecord lr = new LogRecord(Level.SEVERE, loggeableMessage);
		        lr.setParameters(parameters);
		        lr.setThrown(Throwables.getRootCause(exception));
				this.logger.log(lr);
			} else {
				this.logger.log(Level.SEVERE, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void error(Object message, Object... parameters) {
		if (this.logger.isLoggable(Level.SEVERE) && message != null) {
			final String loggeableMessage = message.toString();
			this.logger.log(Level.SEVERE, loggeableMessage, parameters);
		}
	}

	@Override
	public void error(Supplier<String> messageProvider) {
		if (this.logger.isLoggable(Level.SEVERE) && messageProvider != null) {
			this.logger.log(Level.SEVERE, messageProvider);
		}
	}

	@Override
	public void warning(Object message, Throwable exception, Object... parameters) {
		if (this.logger.isLoggable(Level.WARNING) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				if (exception != null) {
					final LogRecord lr = new LogRecord(Level.WARNING, loggeableMessage);
			        lr.setParameters(parameters);
			        lr.setThrown(exception);
					this.logger.log(lr);
				} else {
					this.logger.log(Level.WARNING, loggeableMessage, parameters);
				}
			}
		}
	}

	@Override
	public void warning(Object message, Object... parameters) {
		if (this.logger.isLoggable(Level.WARNING) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				this.logger.log(Level.WARNING, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void warning(Supplier<String> messageProvider) {
		if (this.logger.isLoggable(Level.WARNING) && messageProvider != null) {
			this.logger.log(Level.WARNING, messageProvider);
		}
	}

	@Override
	public void info(Object message, Object... parameters) {
		if (this.logger.isLoggable(Level.INFO) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				this.logger.log(Level.INFO, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void info(Supplier<String> messageProvider) {
		if (this.logger.isLoggable(Level.INFO) && messageProvider != null) {
			this.logger.log(Level.INFO, messageProvider);
		}
	}

	@Override
	public void debug(Object message, Object... parameters) {
		if (this.logger.isLoggable(Level.CONFIG) && message != null) {
			final String loggeableMessage = message.toString();
			if (!loggeableMessage.isEmpty()) {
				this.logger.log(Level.CONFIG, loggeableMessage, parameters);
			}
		}
	}

	@Override
	public void debug(Supplier<String> messageProvider) {
		if (this.logger.isLoggable(Level.CONFIG) && messageProvider != null) {
			this.logger.log(Level.CONFIG, messageProvider);
		}
	}

	@Override
	public boolean isErrorLogEnabled() {
		return this.logger.isLoggable(Level.SEVERE);
	}

	@Override
	public boolean isWarningLogEnabled() {
		return this.logger.isLoggable(Level.WARNING);
	}

	@Override
	public boolean isInfoLogEnabled() {
		return this.logger.isLoggable(Level.INFO);
	}

	@Override
	public boolean isDebugLogEnabled() {
		return this.logger.isLoggable(Level.CONFIG);
	}

	@Override
	public int getLogLevel() {
		return LoggerCreator.toInt(this.logger.getLevel());
	}

	@Override
	public void setLogLevel(int level) {
		this.logger.setLevel(LoggerCreator.fromInt(level));
	}

}
