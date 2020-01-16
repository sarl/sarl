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

package io.janusproject.modules.executors;

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.Executors;
import java.util.concurrent.RejectedExecutionHandler;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import javax.inject.Inject;
import javax.inject.Provider;

import com.google.common.base.Strings;
import com.google.inject.AbstractModule;
import com.google.inject.Singleton;

import io.janusproject.JanusConfig;
import io.janusproject.kernel.services.jdk.executors.JdkExecutorService;
import io.janusproject.kernel.services.jdk.executors.JdkRejectedExecutionHandler;
import io.janusproject.kernel.services.jdk.executors.JdkUncaughtExceptionHandler;
import io.janusproject.services.executor.ExecutorService;

/**
 * Configure the module for the {@code ExecutorService} based on the JDK.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JdkExecutorModule extends AbstractModule {

	@Override
	protected void configure() {
		// Thread catchers
		bind(UncaughtExceptionHandler.class).to(JdkUncaughtExceptionHandler.class).in(Singleton.class);
		bind(RejectedExecutionHandler.class).to(JdkRejectedExecutionHandler.class).in(Singleton.class);

		// Bind the background objects
		//bind(ThreadFactory.class).to(JdkThreadFactory.class).in(Singleton.class);
		//bind(java.util.concurrent.ExecutorService.class).to(JdkThreadPoolExecutor.class).in(Singleton.class);
		//bind(ScheduledExecutorService.class).to(JdkScheduledThreadPoolExecutor.class).in(Singleton.class);
		bind(java.util.concurrent.ExecutorService.class).toProvider(ExecutorProvider.class).in(Singleton.class);
		bind(ScheduledExecutorService.class).toProvider(ScheduledExecutorProvider.class).in(Singleton.class);

		// Bind the service
		bind(ExecutorService.class).to(JdkExecutorService.class).in(Singleton.class);
	}

	/** Provider of a low-level executor service.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ExecutorProvider implements Provider<java.util.concurrent.ExecutorService> {

		private RejectedExecutionHandler rejectedExecutionHandler;

		/** Constructor.
		 */
		public ExecutorProvider() {
			//
		}

		/** Change the handler for rejected executions.
		 *
		 * @param rejectionHandler the handler of rejected tasks.
		 */
		@Inject
		public void setRejectedExecutionHandler(RejectedExecutionHandler rejectionHandler) {
			this.rejectedExecutionHandler = rejectionHandler;
		}

		@Override
		public java.util.concurrent.ExecutorService get() {
			final String maxNumberOfThreadsStr = JanusConfig.getSystemProperty(JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME, null);
			final java.util.concurrent.ExecutorService executor;
			if (!Strings.isNullOrEmpty(maxNumberOfThreadsStr)) {
				// Use a special executor service if Janus was launched with a max number of threads.
				final int minPoolSize = JanusConfig.getSystemPropertyAsInteger(JanusConfig.MIN_NUMBER_OF_THREADS_IN_EXECUTOR_NAME,
						JanusConfig.MIN_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE);
				final int maxPoolSize = toInt(maxNumberOfThreadsStr, JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE);
				final int keepAliveDuration = JanusConfig.getSystemPropertyAsInteger(JanusConfig.THREAD_KEEP_ALIVE_DURATION_NAME,
						JanusConfig.THREAD_KEEP_ALIVE_DURATION_VALUE);
				executor = new ThreadPoolExecutor(
						Math.max(0, Math.min(minPoolSize, maxPoolSize)),
						Math.max(1, Math.max(minPoolSize, maxPoolSize)),
						keepAliveDuration, TimeUnit.SECONDS, new SynchronousQueue<Runnable>());
			} else {
				// Use the default thread executor
				executor = Executors.newFixedThreadPool(JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE);
			}

			if (this.rejectedExecutionHandler != null && executor instanceof ThreadPoolExecutor) {
				((ThreadPoolExecutor) executor).setRejectedExecutionHandler(this.rejectedExecutionHandler);
			}
			return executor;
		}

	    private static int toInt(String value, int defaultValue) {
	        if (value != null) {
	            try {
	                return Integer.parseInt(value);
	            } catch (Throwable exception) {
	                //
	            }
	        }
	        return defaultValue;
	    }

	}

	/** Provider of a low-level scheduled executor service.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ScheduledExecutorProvider implements Provider<ScheduledExecutorService> {

		private RejectedExecutionHandler rejectedExecutionHandler;

		/** Constructor.
		 */
		public ScheduledExecutorProvider() {
			//
		}

		/** Change the handler for rejected executions.
		 *
		 * @param handler the handler.
		 */
		@Inject
		public void setRejectedExecutionHandler(RejectedExecutionHandler handler) {
			this.rejectedExecutionHandler = handler;
		}

		@Override
		public ScheduledExecutorService get() {
			final int minPoolSize = JanusConfig.getSystemPropertyAsInteger(JanusConfig.MIN_NUMBER_OF_THREADS_IN_EXECUTOR_NAME,
					JanusConfig.MIN_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE);
			final int maxPoolSize = JanusConfig.getSystemPropertyAsInteger(JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME,
					JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE);
			final ScheduledExecutorService executor = Executors.newScheduledThreadPool(Math.max(1, Math.min(minPoolSize, maxPoolSize)));
			if (this.rejectedExecutionHandler != null && executor instanceof ThreadPoolExecutor) {
				((ThreadPoolExecutor) executor).setRejectedExecutionHandler(this.rejectedExecutionHandler);
			}
			return executor;
		}

	}

}
