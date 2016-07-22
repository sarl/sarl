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

import java.util.concurrent.Callable;
import java.util.concurrent.Future;
import java.util.concurrent.RunnableScheduledFuture;
import java.util.concurrent.ScheduledThreadPoolExecutor;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.TimeUnit;

import com.google.inject.Inject;
import io.janusproject.JanusConfig;
import io.janusproject.services.executor.ChuckNorrisException;
import io.janusproject.util.ListenerCollection;

/**
 * Executor that support uncaucht exceptions and interruptable threads.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JdkScheduledThreadPoolExecutor extends ScheduledThreadPoolExecutor {

	private ListenerCollection<JdkTaskListener> listeners;

	/**
	 * @param factory - the thread factory to use for creating new threads.
	 */
	@Inject
	public JdkScheduledThreadPoolExecutor(ThreadFactory factory) {
		super(JanusConfig.getSystemPropertyAsInteger(JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME,
				JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE), factory);
	}

	/**
	 * Add a listener on tasks.
	 *
	 * @param listener - the listener on task events
	 */
	public synchronized void addTaskListener(JdkTaskListener listener) {
		if (this.listeners == null) {
			this.listeners = new ListenerCollection<>();
		}
		this.listeners.add(JdkTaskListener.class, listener);
	}

	/**
	 * Remove a listener on tasks.
	 *
	 * @param listener - the listener on task events
	 */
	public synchronized void removeTaskListener(JdkTaskListener listener) {
		if (this.listeners != null) {
			this.listeners.remove(JdkTaskListener.class, listener);
			if (this.listeners.isEmpty()) {
				this.listeners = null;
			}
		}
	}

	/**
	 * Notify the listeners about a task termination.
	 *
	 * @param thread - the thread that was run the finished task.
	 * @param task - the finished task.
	 */
	protected void fireTaskFinished(Thread thread, Runnable task) {
		JdkTaskListener[] listeners;
		synchronized (this) {
			if (this.listeners == null) {
				return;
			}
			listeners = this.listeners.getListeners(JdkTaskListener.class);
		}
		for (JdkTaskListener listener : listeners) {
			listener.taskFinished(thread, task);
		}
	}

	@Override
	protected <V> RunnableScheduledFuture<V> decorateTask(Callable<V> callable, RunnableScheduledFuture<V> task) {
		return new JdkJanusScheduledFutureTask<>(task);
	}

	@Override
	protected <V> RunnableScheduledFuture<V> decorateTask(Runnable runnable, RunnableScheduledFuture<V> task) {
		return new JdkJanusScheduledFutureTask<>(task);
	}

	@Override
	public <T> Future<T> submit(Runnable task, T result) {
		return schedule(new ResultRunnable<>(task, result), 0, TimeUnit.NANOSECONDS);
	}

	@Override
	protected void beforeExecute(Thread thread, Runnable runnable) {
		// Was the task submitted (if future task) or executed?
		if (runnable instanceof JdkJanusScheduledFutureTask<?>) {
			((JdkJanusScheduledFutureTask<?>) runnable).setThread(thread);
		}
	}

	@Override
	protected void afterExecute(Runnable runnable, Throwable thread) {
		assert (thread == null);
		assert (runnable instanceof JdkJanusScheduledFutureTask<?>);
		JdkJanusScheduledFutureTask<?> task = (JdkJanusScheduledFutureTask<?>) runnable;
		assert (task.isDone() || task.isCancelled() || task.isPeriodic());
		if (task.isDone() || task.isCancelled()) {
			task.reportException(task.getThread());
			fireTaskFinished(task.getThread(), task);
		}
	}

	/**
	 * Implementation of the result of a runnable.
	 *
	 * @param <V> type of the result.
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private static class ResultRunnable<V> implements Callable<V> {

		private final Runnable runnable;

		private final V result;

		/**
		 * Construct.
		 *
		 * @param runnable the runnable.
		 * @param result the result.
		 */
		ResultRunnable(Runnable runnable, V result) {
			this.runnable = runnable;
			this.result = result;
		}

		@Override
		public V call() throws Exception {
			try {
				this.runnable.run();
			} catch (ChuckNorrisException exception) {
				//
			}
			return this.result;
		}

	}

}
