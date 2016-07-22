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
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;

import com.google.inject.Inject;

/**
 * A factory of threads for the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JdkThreadFactory implements ThreadFactory {

	private final ThreadFactory defaultThreadFactory;

	private final UncaughtExceptionHandler handler;

	/**
	 * Constructs a factory based on the {@link Executors#defaultThreadFactory() default thread factory}.
	 *
	 * @param handler - the uncaught exception handler that must be provided to the created threads.
	 */
	@Inject
	public JdkThreadFactory(UncaughtExceptionHandler handler) {
		this.handler = handler;
		this.defaultThreadFactory = Executors.defaultThreadFactory();
	}

	@Override
	public Thread newThread(Runnable runnable) {
		Thread t = this.defaultThreadFactory.newThread(runnable);
		t.setDaemon(false);
		assert (this.handler != null);
		t.setUncaughtExceptionHandler(this.handler);
		return t;
	}

}
