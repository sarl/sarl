/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
import java.util.logging.Level;
import java.util.logging.Logger;

import com.google.common.base.Strings;
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

	/**
	 * @param agent - owner of this skill.
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

	/**
	 * Replies the logger.
	 *
	 * @return the logger.
	 */
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
	public void error(Object message, Throwable exception) {
		if (this.logger.isLoggable(Level.SEVERE)) {
			final String loggableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			if (exception != null) {
				this.logger.log(Level.SEVERE, loggableMessage, exception);
			} else {
				this.logger.log(Level.SEVERE, loggableMessage);
			}
		}
	}

	@Override
	public void error(Object message) {
		if (this.logger.isLoggable(Level.SEVERE)) {
			final String loggeableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			this.logger.log(Level.SEVERE, loggeableMessage);
		}
	}

	@Override
	public void warning(Object message, Throwable exception) {
		if (this.logger.isLoggable(Level.WARNING)) {
			final String loggeableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			if (exception != null) {
				this.logger.log(Level.WARNING, loggeableMessage, exception);
			} else {
				this.logger.log(Level.WARNING, loggeableMessage);
			}
		}
	}

	@Override
	public void warning(Object message) {
		if (this.logger.isLoggable(Level.WARNING)) {
			final String loggeableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			this.logger.log(Level.WARNING, loggeableMessage);
		}
	}

	@Override
	public void info(Object message) {
		if (this.logger.isLoggable(Level.INFO)) {
			final String loggeableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			this.logger.log(Level.INFO, loggeableMessage);
		}
	}

	@Override
	public void debug(Object message) {
		if (this.logger.isLoggable(Level.CONFIG)) {
			final String loggeableMessage = Strings.nullToEmpty(message == null ? null : message.toString());
			this.logger.log(Level.CONFIG, loggeableMessage);
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
