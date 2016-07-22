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

import java.lang.ref.WeakReference;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;
import java.util.concurrent.atomic.AtomicBoolean;

import io.janusproject.services.executor.ChuckNorrisException;
import io.janusproject.services.executor.JanusFutureTask;

/**
 * A {@link FutureTask} that is {@link Runnable}. Successful execution of the <tt>run</tt> method causes completion of the
 * <tt>Future</tt> and allows access to its results.
 *
 * @param <V> The type of the return value.
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see FutureTask
 */
class JdkJanusFutureTask<V> extends FutureTask<V> implements JanusFutureTask<V> {

	private static final int TIMEOUT = 10;

	private final AtomicBoolean treated = new AtomicBoolean(false);

	private WeakReference<Thread> thread;

	private final WeakReference<Object> subruntime;

	/**
	 * Creates a {@code FutureTask} that will, upon running, execute the given {@code Runnable}, and arrange that {@code get} will
	 * return the given result on successful completion.
	 *
	 * @param runnable - the runnable task
	 * @param result - the result to return on successful completion. If you don't need a particular result, consider using
	 *        constructions of the form: {@code Future<?> f = new FutureTask<Void>(runnable, null)}
	 * @throws NullPointerException - if the runnable is <code>null</code>
	 */
	JdkJanusFutureTask(Runnable runnable, V result) throws NullPointerException {
		super(runnable, result);
		this.subruntime = new WeakReference<>(runnable);
	}

	/**
	 * Creates a FutureTask that will, upon running, execute the given Callable.
	 *
	 * @param callable - the callable task.
	 * @throws NullPointerException if the callable is null
	 */
	JdkJanusFutureTask(Callable<V> callable) throws NullPointerException {
		super(callable);
		this.subruntime = new WeakReference<>(callable);
	}

	@Override
	public String toString() {
		Object object = this.subruntime.get();
		if (object != null) {
			object = object.toString();
		}
		return "[ " + object + " ] ON [ " + getThread() + " ]"; //$NON-NLS-1$//$NON-NLS-2$//$NON-NLS-3$
	}

	@Override
	protected void done() {
		if (!this.treated.getAndSet(true)) {
			// Test the throw of an exception
			try {
				// This function should not timeout because the task should be terminated.
				get(TIMEOUT, TimeUnit.SECONDS);
			} catch (Throwable e) {
				JdkExecutorUtil.log(getThread(), e);
			}
		}
	}

	@Override
	public V get() throws InterruptedException, ExecutionException {
		try {
			return super.get();
		} catch (ExecutionException e) {
			Throwable ex = e;
			while (ex instanceof ExecutionException) {
				ex = e.getCause();
			}
			if (ex instanceof ChuckNorrisException) {
				return null;
			}
			throw e;
		}
	}

	@Override
	public V get(long timeout, TimeUnit unit) throws InterruptedException, ExecutionException, TimeoutException {
		try {
			return super.get(timeout, unit);
		} catch (ExecutionException e) {
			Throwable ex = e;
			while (ex instanceof ExecutionException) {
				ex = e.getCause();
			}
			if (ex instanceof ChuckNorrisException) {
				return null;
			}
			throw e;
		}
	}

	/**
	 * Set the running thread.
	 *
	 * @param thread - thread that is running the task.
	 */
	void setThread(Thread thread) {
		this.thread = new WeakReference<>(thread);
	}

	@Override
	public Thread getThread() {
		return this.thread.get();
	}

	@Override
	public boolean isCurrentThread() {
		return Thread.currentThread() == this.thread.get();
	}

}
