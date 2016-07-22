/*
 * $Id$
 *
 * Janus platform is an open-source multiagent platform.
 * More details on http://www.janusproject.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.janusproject.kernel.services.jdk.executors;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.ExecutionException;

import io.janusproject.services.executor.ChuckNorrisException;

/**
 * Utilities for executors.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
final class JdkExecutorUtil {

	private JdkExecutorUtil() {
		//
	}

	/**
	 * Log the exception.
	 *
	 * @param thread - the thread in which the exception occurs.
	 * @param exception - the exception to log, or <code>null</code> if none.
	 * @return <code>true</code> if ChuckNorrisException is detected.
	 */
	@SuppressWarnings("checkstyle:regexp")
	public static boolean log(Thread thread, Throwable exception) {
		if (exception != null) {
			Throwable realException = exception;
			// Get the cause of the exception
			while (realException instanceof ExecutionException) {
				realException = ((ExecutionException) realException).getCause();
			}
			if (!(realException instanceof ChuckNorrisException)) {
				// Call the exception catcher
				UncaughtExceptionHandler handler = thread.getUncaughtExceptionHandler();
				if (handler == null) {
					handler = Thread.getDefaultUncaughtExceptionHandler();
				}
				if (handler != null) {
					handler.uncaughtException(thread, realException);
				} else {
					System.err.println(realException.toString());
					realException.printStackTrace();
				}
			} else {
				return true;
			}
		}
		return false;
	}

}
