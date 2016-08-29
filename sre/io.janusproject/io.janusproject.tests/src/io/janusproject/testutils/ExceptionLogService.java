/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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
package io.janusproject.testutils;

import io.janusproject.services.logging.LogService;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.util.concurrent.AbstractService;
import com.google.common.util.concurrent.Service;

/**
 * This class provides an implementation of the {@link LogService} that throws an exception when logging an error.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ExceptionLogService extends AbstractService implements LogService {

	private final List<Object> results;
	private final Logger logger;

	/**
	 * @param results - the results of the run.
	 */
	public ExceptionLogService(List<Object> results) {
		this.results = results;
		this.logger = Logger.getLogger(ExceptionLogService.class.getName());
	}

	@Override
	public Class<? extends Service> getServiceType() {
		return LogService.class;
	}

	@Override
	public Collection<Class<? extends Service>> getServiceDependencies() {
		return Collections.emptyList();
	}

	@Override
	public Collection<Class<? extends Service>> getServiceWeakDependencies() {
		return Collections.emptyList();
	}

	@Override
	public void info(String messageKey, Object... message) {
		//
	}

	@Override
	public void info(Class<?> propertyType, String messageKey, Object... message) {
		//
	}

	@Override
	public void fineInfo(String messageKey, Object... message) {
		//
	}

	@Override
	public void fineInfo(Class<?> propertyType, String messageKey, Object... message) {
		//
	}

	@Override
	public void finerInfo(String messageKey, Object... message) {
		//
	}

	@Override
	public void finerInfo(Class<?> propertyType, String messageKey, Object... message) {
		//
	}

	@Override
	public void debug(String messageKey, Object... message) {
		//
	}

	@Override
	public void debug(Class<?> propertyType, String messageKey, Object... message) {
		//
	}

	@Override
	public void warning(Class<?> propertyType, String messageKey, Object... message) {
		//
	}

	@Override
	public void warning(String messageKey, Object... message) {
		//
	}

	@Override
	public void error(String messageKey, Object... message) {
		this.results.add(new LoggedException(messageKey));
	}

	@Override
	public void error(Class<?> propertyType, String messageKey, Object... message) {
		this.results.add(new LoggedException(messageKey));
	}

	@Override
	public void log(LogRecord record) {
		if (record.getLevel() == Level.SEVERE) {
			this.results.add(new LoggedException(record.getMessage(), record.getThrown()));
		}
	}

	@Override
	public void log(Level level, Class<?> propertyType, String messageKey, Object... message) {
		if (level == Level.SEVERE) {
			this.results.add(new LoggedException(messageKey));
		}
	}

	@Override
	public void log(Level level, String messageKey, Object... message) {
		if (level == Level.SEVERE) {
			this.results.add(new LoggedException(messageKey));
		}
	}

	@Override
	public Logger getLogger() {
		return this.logger;
	}

	@Override
	public void setLogger(Logger logger) {
		//
	}

	@Override
	public void setFilter(Filter filter) {
		//
	}

	@Override
	public Filter getFilter() {
		return null;
	}

	@Override
	public boolean isLoggeable(Level level) {
		return level == Level.SEVERE;
	}

	@Override
	public Level getLevel() {
		return Level.SEVERE;
	}

	@Override
	public void setLevel(Level level) {
		//
	}

	@Override
	protected void doStart() {
		this.logger.setLevel(Level.OFF);
		notifyStarted();
	}

	@Override
	protected void doStop() {
		this.logger.setLevel(Level.OFF);
		notifyStopped();
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LoggedException extends RuntimeException {

		private static final long serialVersionUID = 7747342758657910404L;

		/**
		 * @param message - the error message.
		 */
		public LoggedException(String message) {
			super(message);
		}

		/**
		 * @param cause - the cause of the error..
		 */
		public LoggedException(Throwable cause) {
			super(cause);
		}

		/**
		 * @param message - the error message.
		 * @param cause - the cause of the error..
		 */
		public LoggedException(String message, Throwable cause) {
			super(message, cause);
		}

	}

}
