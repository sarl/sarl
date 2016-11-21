/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.LogRecord;

import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;
import com.google.inject.Inject;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.executor.JanusScheduledFutureTask;
import io.janusproject.services.logging.LogService;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.core.AgentTask;
import io.sarl.core.Schedules;
import io.sarl.lang.core.Agent;

/**
 * Skill that permits to execute tasks with an executor service.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SchedulesSkill extends BuiltinSkill implements Schedules {

	private static int installationOrder = -1;

	@Inject
	private ExecutorService executorService;

	@Inject
	private LogService logger;

	private final Map<String, AgentTask> tasks = new HashMap<>();

	private final Map<String, Future<?>> futures = new HashMap<>();

	/**
	 * @param agent - the owner of this skill.
	 */
	SchedulesSkill(Agent agent) {
		super(agent);
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	protected String attributesToString() {
		return super.attributesToString() + ", tasks = " + this.tasks; //$NON-NLS-1$
	}

	/**
	 * Remove any reference to the given task.
	 *
	 * @param name - name of the task.
	 */
	private synchronized void finishTask(String name) {
		this.tasks.remove(name);
		this.futures.remove(name);
	}

	/**
	 * Replies the names of the active tasks.
	 *
	 * @return the names of the active tasks.
	 */
	public synchronized Collection<String> getActiveTasks() {
		return new ArrayList<>(this.tasks.keySet());
	}

	/**
	 * Replies the names of the active futures.
	 *
	 * @return the names of the active futures.
	 */
	synchronized Collection<Future<?>> getActiveFutures() {
		return new ArrayList<>(this.futures.values());
	}

	/**
	 * Replies the active future for the task with the given name.
	 *
	 * @param name the name of the task.
	 * @return the active future of the task.
	 */
	synchronized Future<?> getActiveFuture(String name) {
		return this.futures.get(name);
	}

	@Override
	protected synchronized void uninstall() {
		Future<?> future;
		for (final Entry<String, Future<?>> futureDescription : this.futures.entrySet()) {
			future = futureDescription.getValue();
			if ((future instanceof JanusScheduledFutureTask<?>) && ((JanusScheduledFutureTask<?>) future).isCurrentThread()) {
				// Ignore the cancelation of the future.
				// It is assumed that a ChuckNorrisException will be thrown later.
				this.logger.fineInfo(Messages.SchedulesSkill_0,
						futureDescription.getKey(), future);
			} else {
				future.cancel(true);
				this.logger.fineInfo(Messages.SchedulesSkill_1, futureDescription.getKey(), future);
			}
		}
		this.futures.clear();
		this.tasks.clear();
	}

	@Override
	public AgentTask in(long delay, Procedure1<? super Agent> procedure) {
		return in(Schedules.$DEFAULT_VALUE$IN_0, delay, procedure);
	}

	@Override
	public synchronized AgentTask in(AgentTask task, long delay, Procedure1<? super Agent> procedure) {
		final AgentTask rtask = task == null ? task(null) : task;
		rtask.setProcedure(procedure);
		final ScheduledFuture<?> sf = this.executorService.schedule(new AgentRunnableTask(rtask, false), delay, TimeUnit.MILLISECONDS);
		this.futures.put(rtask.getName(), sf);
		return rtask;
	}

	@Override
	public synchronized AgentTask task(String name) {
		final String realName = Strings.isNullOrEmpty(name) ? "task-" + UUID.randomUUID() : name; //$NON-NLS-1$
		if (this.tasks.containsKey(realName)) {
			return this.tasks.get(realName);
		}
		final AgentTask t = new AgentTask();
		t.setName(realName);
		this.tasks.put(realName, t);
		return t;
	}

	@Override
	public final boolean cancel(AgentTask task) {
		return cancel(task, Schedules.$DEFAULT_VALUE$CANCEL_0);
	}

	@Override
	public synchronized boolean cancel(AgentTask task, boolean mayInterruptIfRunning) {
		if (task != null) {
			final String name = task.getName();
			final Future<?> future = this.futures.get(name);
			if (future != null && !future.isDone() && !future.isCancelled() && future.cancel(mayInterruptIfRunning)) {
				finishTask(name);
				return true;
			}
		}
		return false;
	}

	@Override
	public AgentTask every(long period, Procedure1<? super Agent> procedure) {
		return every(Schedules.$DEFAULT_VALUE$EVERY_0, period, procedure);
	}

	@Override
	public synchronized AgentTask every(AgentTask task, long period, Procedure1<? super Agent> procedure) {
		final AgentTask rtask = task == null ? task(null) : task;
		rtask.setProcedure(procedure);
		final ScheduledFuture<?> sf = this.executorService.scheduleAtFixedRate(new AgentRunnableTask(rtask, true), 0, period,
				TimeUnit.MILLISECONDS);
		this.futures.put(rtask.getName(), sf);
		return rtask;
	}

	@Override
	public AgentTask atFixedDelay(long delay, Procedure1<? super Agent> procedure) {
		return atFixedDelay(Schedules.$DEFAULT_VALUE$ATFIXEDDELAY_0, delay, procedure);
	}

	@Override
	public synchronized AgentTask atFixedDelay(AgentTask task, long delay, Procedure1<? super Agent> procedure) {
		final AgentTask rtask = task == null ? task(null) : task;
		rtask.setProcedure(procedure);
		final Future<?> future;
		if (delay <= 0) {
			future = this.executorService.submit(new AgentInfiniteLoopTask(rtask));
		} else {
			future = this.executorService.scheduleWithFixedDelay(
					new AgentRunnableTask(rtask, true),
					0, delay, TimeUnit.MILLISECONDS);
		}
		this.futures.put(rtask.getName(), future);
		return rtask;
	}

	@Override
	public AgentTask execute(Procedure1<? super Agent> procedure) {
		return execute(Schedules.$DEFAULT_VALUE$EXECUTE_0, procedure);
	}

	/**
	 * Implementation of an agent task.
	 *
	 * @author $Author: srodriguez$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("synthetic-access")
	private class AgentRunnableTask implements Runnable {
		private WeakReference<AgentTask> agentTaskRef;

		private final boolean isPeriodic;

		AgentRunnableTask(AgentTask task, boolean isPeriodic) {
			this.agentTaskRef = new WeakReference<>(task);
			this.isPeriodic = isPeriodic;
		}

		@Override
		public void run() {
			final AgentTask task = this.agentTaskRef.get();
			if (task == null) {
				throw new RuntimeException(Messages.SchedulesSkill_2);
			}
			boolean hasError = false;
			try {
				final Agent owner = getOwner();
				final Function1<Agent, Boolean> guard = task.getGuard();
				if (guard == null || guard.apply(owner).booleanValue()) {
					final Procedure1<? super Agent> procedure = task.getProcedure();
					if (procedure != null) {
						procedure.apply(owner);
					}
				}
			} catch (Throwable ex) {
				final LogRecord record = new LogRecord(Level.SEVERE,
						MessageFormat.format(Messages.SchedulesSkill_3, toString(), ex.getLocalizedMessage()));
				record.setThrown(ex);
				SchedulesSkill.this.logger.log(record);
				hasError = true;
			} finally {
				if (hasError || !this.isPeriodic) {
					finishTask(task.getName());
				}
			}
		}

		@Override
		public String toString() {
			return MoreObjects.toStringHelper(this).add("name", this.agentTaskRef.get().getName()) //$NON-NLS-1$
					.add("agent", getOwner().getID()).toString(); //$NON-NLS-1$
		}

	}

	/**
	 * Implementation of an agent infinite loop task.
	 *
	 * @author $Author: srodriguez$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("synthetic-access")
	private class AgentInfiniteLoopTask implements Runnable {
		private WeakReference<AgentTask> agentTaskRef;

		AgentInfiniteLoopTask(AgentTask task) {
			this.agentTaskRef = new WeakReference<>(task);
		}

		private boolean canRun() {
			final AgentTask task = this.agentTaskRef.get();
			if (task != null) {
				final Future<?> future = getActiveFuture(task.getName());
				return future != null && !future.isDone() && !future.isCancelled();
			}
			return false;
		}

		private Function1<Agent, Boolean> getGuard() {
			final AgentTask task = this.agentTaskRef.get();
			if (task != null) {
				return task.getGuard();
			}
			return null;
		}

		private Procedure1<? super Agent> getProcedure() {
			final AgentTask task = this.agentTaskRef.get();
			if (task != null) {
				return task.getProcedure();
			}
			return null;
		}

		@Override
		public void run() {
			try {
				final Agent owner = getOwner();
				while (canRun()) {
					final Function1<Agent, Boolean> guard = getGuard();
					if (guard == null || guard.apply(owner).booleanValue()) {
						final Procedure1<? super Agent> procedure = getProcedure();
						if (procedure != null) {
							procedure.apply(owner);
						}
					}
					Thread.yield();
				}
			} catch (Throwable ex) {
				final LogRecord record = new LogRecord(Level.SEVERE,
						MessageFormat.format(Messages.SchedulesSkill_3, toString(), ex.getLocalizedMessage()));
				record.setThrown(ex);
				SchedulesSkill.this.logger.log(record);
			} finally {
				final AgentTask task = this.agentTaskRef.get();
				if (task != null) {
					finishTask(task.getName());
				}
			}
		}

		@Override
		public String toString() {
			return MoreObjects.toStringHelper(this).add("name", this.agentTaskRef.get().getName()) //$NON-NLS-1$
					.add("agent", getOwner().getID()).toString(); //$NON-NLS-1$
		}

	}

}
