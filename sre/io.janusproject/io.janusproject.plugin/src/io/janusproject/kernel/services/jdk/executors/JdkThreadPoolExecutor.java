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
import java.util.concurrent.RunnableFuture;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import com.google.inject.Inject;
import io.janusproject.JanusConfig;
import io.janusproject.util.ListenerCollection;

/**
 * Executor that support uncaucht exceptions and interruptable threads.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JdkThreadPoolExecutor extends ThreadPoolExecutor {

	private static final long TIMEOUT = 60;

	private ListenerCollection<JdkTaskListener> listeners;

	/**
	 * @param factory - the factory to use for creating new threads.
	 */
	@Inject
	public JdkThreadPoolExecutor(ThreadFactory factory) {
		this(JanusConfig.getSystemPropertyAsInteger(JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_NAME,
				JanusConfig.MAX_NUMBER_OF_THREADS_IN_EXECUTOR_VALUE), factory);
	}

	/**
	 * @param poolSize - maximal number of threads in the pool.
	 * @param factory - thread factory.
	 */
	public JdkThreadPoolExecutor(int poolSize, ThreadFactory factory) {
		super(poolSize, Integer.MAX_VALUE, TIMEOUT, TimeUnit.SECONDS, new SynchronousQueue<Runnable>(), factory);
	}

	/**
	 * Add a listener on tasks.
	 *
	 * @param listener - the listener on task events.
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
	 * @param listener - the listener on task events.
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
	protected <T> RunnableFuture<T> newTaskFor(Callable<T> callable) {
		// This function is invoked when the task was submited
		return new JdkJanusFutureTask<>(callable);
	}

	@Override
	protected <T> RunnableFuture<T> newTaskFor(Runnable runnable, T value) {
		// This function is invoked when the task was submited
		return new JdkJanusFutureTask<>(runnable, value);
	}

	@Override
	protected void beforeExecute(Thread thread, Runnable runnable) {
		// Was the task submitted (if future task) or executed?
		if (runnable instanceof JdkJanusFutureTask<?>) {
			((JdkJanusFutureTask<?>) runnable).setThread(thread);
		}
	}

	@Override
	protected void afterExecute(Runnable runnable, Throwable throwable) {
		Thread th;
		JdkJanusFutureTask<?> task;
		if (runnable instanceof JdkJanusFutureTask<?>) {
			task = (JdkJanusFutureTask<?>) runnable;
			th = task.getThread();
		} else {
			task = null;
			th = Thread.currentThread();
		}
		if (throwable != null && task != null) {
			// Was the task submitted (if future task) or executed?
			JdkExecutorUtil.log(th, throwable);
		}
		fireTaskFinished(th, runnable);
	}

}
