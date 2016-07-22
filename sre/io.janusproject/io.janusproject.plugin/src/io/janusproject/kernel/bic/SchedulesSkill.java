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

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashMap;
import java.util.Map;
import java.util.Map.Entry;
import java.util.UUID;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.google.common.base.MoreObjects;
import com.google.inject.Inject;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.executor.JanusScheduledFutureTask;
import io.janusproject.services.logging.LogService;
import org.arakhne.afc.vmutil.locale.Locale;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;

import io.sarl.core.AgentTask;
import io.sarl.core.Schedules;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.Skill;

/**
 * Skill that permits to execute tasks with an executor service.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class SchedulesSkill extends Skill implements Schedules {

	@Inject
	private ExecutorService executorService;

	@Inject
	private LogService logger;

	private final Map<String, AgentTask> tasks = new HashMap<>();

	private final Map<String, ScheduledFuture<?>> futures = new HashMap<>();

	/**
	 * @param agent - the owner of this skill.
	 */
	SchedulesSkill(Agent agent) {
		super(agent);
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
	synchronized Collection<String> getActiveTasks() {
		return new ArrayList<>(this.tasks.keySet());
	}

	/**
	 * Replies the names of the active futures.
	 *
	 * @return the names of the active futures.
	 */
	synchronized Collection<ScheduledFuture<?>> getActiveFutures() {
		return new ArrayList<>(this.futures.values());
	}

	@Override
	protected synchronized void uninstall() {
		ScheduledFuture<?> future;
		for (Entry<String, ScheduledFuture<?>> futureDescription : this.futures.entrySet()) {
			future = futureDescription.getValue();
			if ((future instanceof JanusScheduledFutureTask<?>) && ((JanusScheduledFutureTask<?>) future).isCurrentThread()) {
				// Ignore the cancelation of the future.
				// It is assumed that a ChuckNorrisException will be thrown later.
				this.logger.fineInfo(SchedulesSkill.class, "SKIP_CANCELED_TASK_ON_CURRENT_THREAD", //$NON-NLS-1$
						futureDescription.getKey(), future);
			} else {
				future.cancel(true);
				this.logger.fineInfo(SchedulesSkill.class, "CANCELED_TASK", futureDescription.getKey(), future); //$NON-NLS-1$
			}
		}
		this.futures.clear();
		this.tasks.clear();
	}

	@Override
	public AgentTask in(long delay, Procedure1<? super Agent> procedure) {
		return in(task("task-" + UUID.randomUUID()), delay, procedure); //$NON-NLS-1$
	}

	@Override
	public synchronized AgentTask in(AgentTask task, long delay, Procedure1<? super Agent> procedure) {
		task.setProcedure(procedure);
		ScheduledFuture<?> sf = this.executorService.schedule(new AgentRunnableTask(task, false), delay, TimeUnit.MILLISECONDS);
		this.futures.put(task.getName(), sf);
		return task;
	}

	@Override
	public synchronized AgentTask task(String name) {
		if (this.tasks.containsKey(name)) {
			return this.tasks.get(name);
		}
		final AgentTask t = new AgentTask();
		t.setName(name);
		t.setGuard(new Function1<Agent, Boolean>() {

			@Override
			public Boolean apply(Agent arg0) {
				return Boolean.TRUE;
			}
		});
		this.tasks.put(name, t);
		return t;
	}

	@Override
	public final boolean cancel(AgentTask task) {
		return cancel(task, true);
	}

	@Override
	public synchronized boolean cancel(AgentTask task, boolean mayInterruptIfRunning) {
		if (task != null) {
			String name = task.getName();
			ScheduledFuture<?> future = this.futures.get(name);
			if (future != null && !future.isDone() && !future.isCancelled() && future.cancel(mayInterruptIfRunning)) {
				finishTask(name);
			}
		}
		return false;
	}

	@Override
	public AgentTask every(long period, Procedure1<? super Agent> procedure) {
		return every(task("task-" + UUID.randomUUID()), period, procedure); //$NON-NLS-1$
	}

	@Override
	public synchronized AgentTask every(AgentTask task, long period, Procedure1<? super Agent> procedure) {
		task.setProcedure(procedure);
		ScheduledFuture<?> sf = this.executorService.scheduleAtFixedRate(new AgentRunnableTask(task, true), 0, period,
				TimeUnit.MILLISECONDS);
		this.futures.put(task.getName(), sf);
		return task;
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
			AgentTask task = this.agentTaskRef.get();
			if (task == null) {
				throw new RuntimeException(Locale.getString(SchedulesSkill.class, "NULL_AGENT_TASK")); //$NON-NLS-1$
			}
			try {
				Agent owner = getOwner();
				if (task.getGuard().apply(owner).booleanValue()) {
					task.getProcedure().apply(owner);
				}
			} finally {
				if (!this.isPeriodic) {
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
