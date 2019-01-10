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

import java.lang.Thread.UncaughtExceptionHandler;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.atomic.AtomicInteger;

import com.google.common.util.concurrent.Service;
import com.google.inject.Inject;
import com.google.inject.Singleton;

import io.janusproject.JanusConfig;
import io.janusproject.services.AbstractDependentService;
import io.janusproject.services.executor.JanusCallable;
import io.janusproject.services.executor.JanusRunnable;

/**
 * Platform service that supports the execution resources.
 *
 * <p>This service is thread-safe.
 *
 * @author $Author: srodriguez$
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@Singleton
public class JdkExecutorService extends AbstractDependentService implements io.janusproject.services.executor.ExecutorService {

	private ScheduledExecutorService schedules;

	private ExecutorService exec;

	private ScheduledFuture<?> purgeTask;

	private UncaughtExceptionHandler uncaughtExceptionHandler;

	/**
	 * Construct.
	 */
	public JdkExecutorService() {
		//
	}

	/**
	 * Change the JRE service for scheduled tasks.
	 *
	 * @param service the JRE service.
	 */
	@Inject
	void setScheduledExecutorService(ScheduledExecutorService service) {
		this.schedules = service;
	}

	/**
	 * Change the JRE service for scheduled tasks.
	 *
	 * @param service the JRE service.
	 */
	@Inject
	void setExecutorService(ExecutorService service) {
		this.exec = service;
	}

	/**
	 * Change the default exception handler.
	 *
	 * @param handler the default exception handler.
	 */
	@Inject
	void setUncaughtExceptionHandler(UncaughtExceptionHandler handler) {
		this.uncaughtExceptionHandler = handler;
	}

	@Override
	public final Class<? extends Service> getServiceType() {
		return io.janusproject.services.executor.ExecutorService.class;
	}

	@Override
	protected void doStart() {
		assert this.schedules != null;
		assert this.exec != null;
		if (this.uncaughtExceptionHandler != null) {
			Thread.setDefaultUncaughtExceptionHandler(this.uncaughtExceptionHandler);
		}
		// Launch a periodic task that is purging the executor pools.
		if ((this.schedules instanceof ThreadPoolExecutor) || (this.exec instanceof ThreadPoolExecutor)) {
			final int delay = JanusConfig.getSystemPropertyAsInteger(JanusConfig.KERNEL_THREAD_PURGE_DELAY_NAME,
					JanusConfig.KERNEL_THREAD_PURGE_DELAY_VALUE);
			this.purgeTask = this.schedules.scheduleWithFixedDelay(new Purger(), delay, delay, TimeUnit.SECONDS);
		}
		notifyStarted();
	}

	@Override
	protected void doStop() {
		if (this.purgeTask != null) {
			this.purgeTask.cancel(true);
			this.purgeTask = null;
		}
		this.exec.shutdown();
		this.schedules.shutdown();
		try {
			final int timeout = JanusConfig.getSystemPropertyAsInteger(JanusConfig.KERNEL_THREAD_TIMEOUT_NAME,
					JanusConfig.KERNEL_THREAD_TIMEOUT_VALUE);
			this.schedules.awaitTermination(timeout, TimeUnit.SECONDS);
			this.exec.awaitTermination(timeout, TimeUnit.SECONDS);
		} catch (InterruptedException e) {
			// This error may occur when the thread is killed before this
			// function is waiting for its termination.
		} finally {
			this.schedules.shutdownNow();
			this.exec.shutdownNow();
			notifyStopped();
		}

	}

	/** Create a task with the given runnable.
	 *
	 * @param runnable the runnable.
	 * @return the task.
	 */
	@SuppressWarnings("static-method")
	protected Runnable createTask(Runnable runnable) {
		if (runnable instanceof JanusRunnable) {
			return runnable;
		}
		return new JanusRunnable(runnable);
	}

	/** Create a task with the given callable.
	 *
	 * @param <T> the type of the returned value.
	 * @param callable the callable.
	 * @return the task.
	 */
	@SuppressWarnings("static-method")
	protected <T> Callable<T> createTask(Callable<T> callable) {
		if (callable instanceof JanusCallable<?>) {
			return callable;
		}
		return new JanusCallable<>(callable);
	}

	@Override
	public void execute(Runnable task) {
		this.exec.execute(createTask(task));
	}

	@Override
	public int executeMultipleTimesInParallelAndWaitForTermination(Runnable task, int nbExecutions, int runGroupSize) throws InterruptedException {
		assert runGroupSize >= 1;
		final Runnable janusTask = createTask(task);
		if (nbExecutions > 1) {
			final AtomicInteger errors = new AtomicInteger();
			final CountDownLatch doneSignal = new CountDownLatch(nbExecutions);
			if (runGroupSize > 1) {
				final int numberOfGroups = nbExecutions / runGroupSize;
				final int rest = nbExecutions - numberOfGroups * runGroupSize;
				for (int i = 0; i < numberOfGroups; ++i) {
					this.exec.execute(() -> {
						for (int j = 0; j < runGroupSize; ++j) {
							try {
								janusTask.run();
							} catch (Throwable e) {
								errors.incrementAndGet();
							} finally {
								doneSignal.countDown();
							}
						}
					});
				}
				if (rest > 1) {
					this.exec.execute(() -> {
						for (int j = 0; j < rest; ++j) {
							try {
								janusTask.run();
							} catch (Throwable e) {
								errors.incrementAndGet();
							} finally {
								doneSignal.countDown();
							}
						}
					});
				}
			} else {
				for (int i = 0; i < nbExecutions; ++i) {
					this.exec.execute(() -> {
						try {
							janusTask.run();
						} catch (Throwable e) {
							errors.incrementAndGet();
						} finally {
							doneSignal.countDown();
						}
					});
				}
			}
			// Wait for all creators to complete before continuing
			doneSignal.await();
			return nbExecutions - errors.get();
		}
		if (nbExecutions == 1) {
			janusTask.run();
			return 1;
		}
		return 0;
	}

	@Override
	public Future<?> submit(Runnable task) {
		return this.exec.submit(createTask(task));
	}

	@Override
	public <T> Future<T> submit(Runnable task, T result) {
		return this.exec.submit(createTask(task), result);
	}

	@Override
	public <T> Future<T> submit(Callable<T> task) {
		return this.exec.submit(createTask(task));
	}

	@Override
	public ScheduledFuture<?> schedule(Runnable command, long delay, TimeUnit unit) {
		return this.schedules.schedule(createTask(command), delay, unit);
	}

	@Override
	public <T> ScheduledFuture<T> schedule(Callable<T> command, long delay, TimeUnit unit) {
		return this.schedules.schedule(createTask(command), delay, unit);
	}

	@Override
	public ScheduledFuture<?> scheduleAtFixedRate(Runnable command, long initialDelay, long period, TimeUnit unit) {
		return this.schedules.scheduleAtFixedRate(createTask(command), initialDelay, period, unit);
	}

	@Override
	public ScheduledFuture<?> scheduleWithFixedDelay(Runnable command, long initialDelay, long delay, TimeUnit unit) {
		return this.schedules.scheduleWithFixedDelay(createTask(command), initialDelay, delay, unit);
	}

	@Override
	public ExecutorService getExecutorService() {
		return this.exec;
	}

	@Override
	public void purge() {
		if (this.exec instanceof ThreadPoolExecutor) {
			((ThreadPoolExecutor) this.exec).purge();
		}
		if (this.schedules instanceof ThreadPoolExecutor) {
			((ThreadPoolExecutor) this.schedules).purge();
		}
	}

	/**
	 * Task that is purging the thread pools.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class Purger implements Runnable {

		private String oldThreadName;

		/**
		 * Construct.
		 */
		Purger() {
			//
		}

		private boolean setName() {
			if (this.oldThreadName != null) {
				return false;
			}
			final Thread t = Thread.currentThread();
			this.oldThreadName = t.getName();
			t.setName(toString());
			return true;
		}

		private boolean restoreName() {
			if (this.oldThreadName == null) {
				return false;
			}
			final Thread t = Thread.currentThread();
			t.setName(this.oldThreadName);
			this.oldThreadName = null;
			return true;
		}

		@Override
		public void run() {
			assert setName();
			try {
				purge();
			} finally {
				assert restoreName();
			}
		}

		@Override
		public String toString() {
			return "Janus Thread Purger"; //$NON-NLS-1$
		}

	}

}
