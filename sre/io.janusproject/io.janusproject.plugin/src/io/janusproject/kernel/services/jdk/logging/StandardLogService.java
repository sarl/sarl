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

package io.janusproject.kernel.services.jdk.logging;

import java.util.logging.LogRecord;
import java.util.logging.Logger;

import com.google.common.base.Strings;
import com.google.common.util.concurrent.Service;

import io.janusproject.JanusConfig;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.logging.LogService;
import io.janusproject.util.LoggerCreator;

/**
 * This class enables to log information by ensuring that the values of the parameters are not evaluated until the information
 * should be really log, according to the log level. The logger is injected.
 *
 * <p>The LogService considers the parameters of the functions as:<ul>
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
 * <p>This service is thread-safe.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class StandardLogService extends AbstractDependentService implements LogService {

	private Logger platformLogger;

	private Logger kernelLogger;

	/**
	 * Construct the service.
	 */
	public StandardLogService() {
		//
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return LogService.class;
	}

	@Override
	public Logger getPlatformLogger() {
		if (this.platformLogger == null) {
			this.platformLogger = LoggerCreator.createPlatformLogger();
		}
		return this.platformLogger;
	}

	@Override
	public Logger getKernelLogger() {
		if (this.kernelLogger == null) {
			this.kernelLogger = LoggerCreator.createModuleLogger(JanusConfig.JANUS_DEFAULT_PLATFORM_NAME, getPlatformLogger());
		}
		return this.kernelLogger;
	}

	@Override
	public LogRecord prepareLogRecord(LogRecord record, String loggerName, Throwable exception) {
		final String name = record.getLoggerName();
		if (Strings.isNullOrEmpty(name)) {
			if (!Strings.isNullOrEmpty(loggerName)) {
				record.setLoggerName(loggerName);
			} else {
				record.setLoggerName(JanusConfig.JANUS_DEFAULT_PLATFORM_NAME);
			}
		}
		if (exception != null) {
			record.setThrown(exception);
			final StackTraceElement[] trace = exception.getStackTrace();
			if (trace != null && trace.length > 0) {
				final StackTraceElement elt = trace[0];
				assert elt != null;
				record.setSourceClassName(elt.getClassName());
				record.setSourceMethodName(elt.getMethodName());
			}
		}
		return record;
	}

	@Override
	protected void doStart() {
		notifyStarted();
	}

	@Override
	protected void doStop() {
		notifyStopped();
	}

}
