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

package io.janusproject.modules.hazelcast;

import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.hazelcast.logging.AbstractLogger;
import com.hazelcast.logging.ILogger;
import com.hazelcast.logging.LogEvent;
import com.hazelcast.logging.LoggerFactorySupport;

import io.janusproject.services.logging.LogService;

/**
 * Factory of logger for the Hazelcast API.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class HazelcastKernelLoggerFactory extends LoggerFactorySupport {

	private static LogService logger;

	/**
	 * Construct.
	 */
	public HazelcastKernelLoggerFactory() {
		//
	}

	/**
	 * Replies the log service using by Hazelcast.
	 *
	 * @return the log service.
	 */
	public static LogService getLogService() {
		synchronized (HazelcastKernelLoggerFactory.class) {
			return logger;
		}
	}

	/**
	 * Set the log service used by Hazelcast.
	 *
	 * @param service the log service.
	 */
	public static void setLogService(LogService service) {
		synchronized (HazelcastKernelLoggerFactory.class) {
			logger = service;
		}
	}

	@Override
	protected ILogger createLogger(String name) {
		return new HzKernelLogger();
	}

	/**
	 * Logger for Hazelcast.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static final class HzKernelLogger extends AbstractLogger {

		/**
		 * Construct.
		 */
		HzKernelLogger() {
			//
		}

		@Override
		public void log(Level level, String message) {
			final LogService serv = getLogService();
			if (serv != null) {
				final Logger log = serv.getKernelLogger();
				if (log != null) {
					log.log(new LogRecord(level, message));
				}
			}
		}

		@Override
		public void log(Level level, String message, Throwable thrown) {
			final LogService serv = getLogService();
			if (serv != null && serv.getKernelLogger().isLoggable(level)) {
				final StackTraceElement elt = getCaller();
				assert elt != null;
				final LogRecord record = new LogRecord(level, message);
				if (thrown != null) {
					record.setThrown(thrown);
				}
				record.setSourceClassName(elt.getClassName());
				record.setSourceMethodName(elt.getMethodName());
				serv.getKernelLogger().log(record);
			}
		}

		@Override
		public void log(LogEvent logEvent) {
			final LogService serv = getLogService();
			if (serv != null) {
				serv.getKernelLogger().log(logEvent.getLogRecord());
			}
		}

		@Override
		public Level getLevel() {
			final LogService serv = getLogService();
			if (serv != null) {
				return serv.getKernelLogger().getLevel();
			}
			return Level.OFF;
		}

		@Override
		public boolean isLoggable(Level level) {
			final LogService serv = getLogService();
			if (serv != null) {
				return serv.getKernelLogger().isLoggable(level);
			}
			return false;
		}

		private static StackTraceElement getCaller() {
			final StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
			Class<?> type;
			// Start at 1 because the top of the stack corresponds to getStackTrace.
			for (int i = 1; i < stackTrace.length; ++i) {
				try {
					type = Class.forName(stackTrace[i].getClassName());
					if (type != null && !ILogger.class.isAssignableFrom(type)) {
						return stackTrace[i];
					}
				} catch (ClassNotFoundException e) {
					//
				}
			}
			return null;
		}

	}

}
