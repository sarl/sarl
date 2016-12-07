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

package io.janusproject.kernel.services.jdk.executors;

import java.lang.Thread.UncaughtExceptionHandler;
import java.text.MessageFormat;
import java.util.concurrent.CancellationException;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.janusproject.services.executor.ChuckNorrisException;
import io.janusproject.services.logging.LogService;

import io.sarl.core.Initialize;

/**
 * A factory of threads for the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class JdkUncaughtExceptionHandler implements UncaughtExceptionHandler {

	private final LogService logger;

	/**
	 * @param logger - the logging service that must be used for output the errors.
	 */
	@Inject
	public JdkUncaughtExceptionHandler(LogService logger) {
		assert logger != null;
		this.logger = logger;
	}

	private void log(Throwable exception, String taskId, String taskName) {
		assert exception != null;
		Throwable cause = exception;
		while (cause.getCause() != null && cause.getCause() != cause) {
			cause = cause.getCause();
		}
		final LogRecord record;
		if (cause instanceof ChuckNorrisException || exception instanceof ChuckNorrisException) {
			// Chuck Norris cannot be catched!
			return;
		}
		if (cause instanceof CancellationException || exception instanceof CancellationException) {
			// Avoid too much processing if the error is not loggeable
			if (!this.logger.isLoggeable(Level.FINEST)) {
				return;
			}
			record = new LogRecord(Level.FINEST, MessageFormat.format(Messages.JdkUncaughtExceptionHandler_0, taskId, taskName));
		} else if (cause instanceof InterruptedException || exception instanceof InterruptedException) {
			// Avoid too much processing if the error is not loggeable
			if (!this.logger.isLoggeable(Level.FINEST)) {
				return;
			}
			record = new LogRecord(Level.FINEST, MessageFormat.format(Messages.JdkUncaughtExceptionHandler_1, taskId, taskName));
		} else {
			// Avoid too much processing if the error is not loggeable
			if (!this.logger.isLoggeable(Level.SEVERE)) {
				return;
			}
			record = new LogRecord(Level.SEVERE, MessageFormat.format(Messages.JdkUncaughtExceptionHandler_2,
					cause.getLocalizedMessage(), taskId, taskName));
		}

		record.setThrown(cause);
		final StackTraceElement[] trace = cause.getStackTrace();
		if (trace != null && trace.length > 0) {
			final StackTraceElement elt = trace[0];
			assert elt != null;
			record.setSourceClassName(elt.getClassName());
			record.setSourceMethodName(elt.getMethodName());
		}
		this.logger.log(record);
	}

	@Override
	public void uncaughtException(Thread thread, Throwable exception) {
		log(exception, Long.toString(thread.getId()), thread.getName());
	}

	/**
	 * Replies if the given object is an event that may cause agent stop when an error occured in the event's handler.
	 *
	 * @param object - the event to test.
	 * @return <code>true</code> if the agent must stop if an error occured in the handler for the given event.
	 */
	@SuppressWarnings("static-method")
	public boolean isAutoKillEvent(Object object) {
		return object instanceof Initialize;
	}

}
