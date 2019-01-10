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

package io.janusproject.kernel.services.jdk.executors;

import java.text.MessageFormat;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.ThreadPoolExecutor.CallerRunsPolicy;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.janusproject.services.logging.LogService;

/**
 * A handler for rejected tasks and uncaught exceptions that logs a warning on the platform logger
 * when a task is rejected, and an error for each uncaught exception.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see CallerRunsPolicy
 */
@Singleton
public class JdkRejectedExecutionHandler implements RejectedExecutionHandler {

	private final LogService logger;

	/** Constructor.
	 * @param logger the logging service that must be used for output the errors.
	 */
	@Inject
	public JdkRejectedExecutionHandler(LogService logger) {
		assert logger != null;
		this.logger = logger;
	}

	@Override
	public void rejectedExecution(Runnable task, ThreadPoolExecutor executor) {
		if (!runRejectedTask(task, executor)) {
			final LogRecord record = new LogRecord(Level.FINE,
					MessageFormat.format(Messages.JdkRejectedExecutionHandler_0, task.toString()));
			this.logger.getKernelLogger().log(record);
		}
	}

	/** Run the given task within the current thread if the executor is not shut down.
	 * The task is not run by the given executor. The executor is used for checking if
	 * the executor service is shut down.
	 *
	 * @param runnable the runnable task to be executed
	 * @param executor the executor attempting to give the shut down status
	 * @return {@code true} if the task is run. {@code false} if the task was not run.
	 * @since 0.7
	 * @see CallerRunsPolicy
	 */
	protected static boolean runRejectedTask(Runnable runnable, ThreadPoolExecutor executor) {
		// Runs the task directly in the calling thread of the {@code execute} method,
		// unless the executor has been shut down, in which case the task
		// is discarded.
		if (!executor.isShutdown()) {
			runnable.run();
			return true;
		}
		return false;
	}

}
