/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2017 the original authors or authors.
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

import java.text.MessageFormat;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.janusproject.services.logging.LogService;

/**
 * An execution rejection handler for the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class JdkRejectedExecutionHandler implements RejectedExecutionHandler {

	private final LogService logger;

	/**
	 * @param logger - the logging service that must be used for output the errors.
	 */
	@Inject
	public JdkRejectedExecutionHandler(LogService logger) {
		assert logger != null;
		this.logger = logger;
	}

	@Override
	public void rejectedExecution(Runnable task, ThreadPoolExecutor executor) {
		final LogRecord record = new LogRecord(Level.SEVERE,
				MessageFormat.format(Messages.JdkRejectedExecutionHandler_0, task.toString()));
		this.logger.log(record);
	}

}
