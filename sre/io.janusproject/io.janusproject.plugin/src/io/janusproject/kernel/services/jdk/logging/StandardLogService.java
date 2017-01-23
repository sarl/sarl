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

package io.janusproject.kernel.services.jdk.logging;

import java.text.MessageFormat;
import java.util.logging.Filter;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;

import io.janusproject.JanusConfig;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.logging.LogService;
import io.janusproject.util.ClassFinder;

/**
 * This class enables to log information by ensuring that the values of the parameters are not evaluated until the information
 * should be really log, according to the log level. The logger is injected.
 *
 * <p>The LogService considers the parameters of the functions as:
 * <ul>
 * <li>the message is the the message in the property file;</li>
 * <li>the parameters are the values that will replace the strings {0}, {1}, {2}... in the text extracted from the
 * resource property.</li>
 * </ul>
 *
 * <p>If a <code>Throwable</code> is passed as parameter, the text of the exception is retrieved.
 *
 * <p>If a <code>Callable</code> is passed as parameter, the object is automatically called.
 *
 * <p>If a <code>LogParam</code> is passed as parameter, the <code>toString</code> function will be invoked.
 *
 * <p>For all the other objects, the {@link #toString()} function is invoked.
 *
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardLogService extends AbstractDependentService implements LogService {

	private Logger logger;

	private LoggerCallerProvider loggerCallerProvider = new StackTraceLoggerCallerProvider();

	/**
	 * Construct the service.
	 */
	public StandardLogService() {
		//
	}

	/**
	 * Replies the object that permits to determine the caller of the logger.
	 *
	 * @return the object that permits to determine the caller of the logger.
	 */
	public LoggerCallerProvider getLoggerCaller() {
		return this.loggerCallerProvider;
	}

	/**
	 * Change the object that permits to determine the caller of the logger.
	 *
	 * @param provider - the object that permits to determine the caller of the logger.
	 */
	public void setLoggerCaller(LoggerCallerProvider provider) {
		if (provider == null) {
			this.loggerCallerProvider = new StackTraceLoggerCallerProvider();
		} else {
			this.loggerCallerProvider = provider;
		}
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return LogService.class;
	}

	/**
	 * Replies if this service permits to log the messages.
	 *
	 * @return <code>true</code> if the messages are loggeable, <code>false</code> otherwise.
	 */
	protected boolean isLogEnabled() {
		return state().ordinal() <= State.RUNNING.ordinal();
	}

	private static LogRecord createLogRecord(Level level, String text, boolean exception, Object... message) {
		Throwable realException = null;
		if (exception) {
			for (final Object m : message) {
				if (m instanceof Throwable) {
					realException = (Throwable) m;
					break;
				}
			}
		}
		final LogRecord record = new LogRecord(level, text);
		if (realException != null) {
			record.setThrown(realException);
		}

		if (record.getLoggerName() == null) {
		    record.setLoggerName(JanusConfig.JANUS_DEFAULT_PLATFORM_NAME);
		}
		return record;
	}

	private void writeInLog(Level level, boolean exception, String message, Object... params) {
		if (isLogEnabled() && this.logger.isLoggable(level)) {
			final LoggerCaller caller = this.loggerCallerProvider.getLoggerCaller();
			final String text = MessageFormat.format(message, params);
			final LogRecord record = createLogRecord(level, text, exception, params);
			record.setSourceClassName(caller.getTypeName());
			final String methodName = caller.getMethod();
			if (!Strings.isNullOrEmpty(methodName)) {
				record.setSourceMethodName(methodName);
			}
			this.logger.log(record);
		}
	}

	@Override
	public void log(LogRecord record) {
		if (isLogEnabled()) {
			this.logger.log(record);
		}
	}

	@Override
	public void log(Level level, String message, Object... params) {
		writeInLog(level, true, message, params);
	}

	@Override
	public void info(String message, Object... params) {
		writeInLog(Level.INFO, false, message, params);
	}

	@Override
	public void fineInfo(String message, Object... params) {
		writeInLog(Level.FINE, false, message, params);
	}

	@Override
	public void finerInfo(String message, Object... params) {
		writeInLog(Level.FINER, false, message, params);
	}

	@Override
	public void debug(String message, Object... params) {
		writeInLog(Level.FINEST, true, message, params);
	}

	@Override
	public void warning(String message, Object... params) {
		writeInLog(Level.WARNING, true, message, params);
	}

	@Override
	public void warning(Throwable exception) {
		writeInLog(Level.WARNING, true, exception.getLocalizedMessage(), exception);
	}

	@Override
	public void error(String message, Object... params) {
		writeInLog(Level.SEVERE, true, message, params);
	}

	@Override
	public void error(Throwable exception) {
		writeInLog(Level.SEVERE, true, exception.getLocalizedMessage(), exception);
	}

	@Override
	public Logger getLogger() {
		return this.logger;
	}

	@Inject
	@Override
	public void setLogger(Logger logger) {
		if (logger != null) {
			this.logger = logger;
		}
	}

	@Override
	public void setFilter(Filter filter) {
		this.logger.setFilter(filter);
	}

	@Override
	public Filter getFilter() {
		return this.logger.getFilter();
	}

	@Override
	public boolean isLoggeable(Level level) {
		return isLogEnabled() && this.logger.isLoggable(level);
	}

	@Override
	public Level getLevel() {
		return this.logger.getLevel();
	}

	@Override
	public void setLevel(Level level) {
		this.logger.setLevel(level);
	}

	@Override
	protected void doStart() {
		notifyStarted();
	}

	@Override
	protected void doStop() {
		notifyStopped();
	}

	/**
	 * Provides the type of the caller of the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public interface LoggerCallerProvider {

		/**
		 * Replies the logger caller.
		 *
		 * @return the logger caller.
		 */
		LoggerCaller getLoggerCaller();

	}

	/**
	 * Provides the type of the caller of the logger.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class LoggerCaller {

		private final String methodName;

		private final String className;

		/**
		 * @param className - the type of the caller.
		 * @param methodName - the name of the called method.
		 */
		public LoggerCaller(String className, String methodName) {
			this.className = className;
			this.methodName = methodName;
		}

		/**
		 * Replies the name of the type of the logger caller.
		 *
		 * @return the name of type of the logger caller.
		 */
		public String getTypeName() {
			return this.className;
		}

		/**
		 * Replies the name of the last method encountered in the stack trace.
		 *
		 * @return the name of the last invoked method of the logger caller.
		 */
		public String getMethod() {
			return this.methodName;
		}

	}

	/**
	 * Provider of calling function on the stack trace.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class StackTraceLoggerCallerProvider implements LoggerCallerProvider {

		/**
		 * Construct.
		 */
		public StackTraceLoggerCallerProvider() {
			//
		}

		private static StackTraceElement getStackTraceElement() {
			final StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
			// Start at 1 because the top of the stack corresponds to getStackTrace.
			for (int i = 1; i < stackTrace.length; ++i) {
				final String className = stackTrace[i].getClassName();
				final Class<?> type = ClassFinder.findClass(className);
				if (type != null) {
					return stackTrace[i];
				}
			}
			return null;
		}

		@Override
		public LoggerCaller getLoggerCaller() {
			final StackTraceElement element = getStackTraceElement();
			if (element != null) {
				return new LoggerCaller(element.getClassName(), element.getMethodName());
			}
			return null;
		}

	}

}
