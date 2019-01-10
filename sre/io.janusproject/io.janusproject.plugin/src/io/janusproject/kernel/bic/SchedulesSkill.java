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

package io.janusproject.kernel.bic;

import java.lang.ref.WeakReference;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Objects;
import java.util.TreeMap;
import java.util.UUID;
import java.util.concurrent.Future;
import java.util.concurrent.ScheduledFuture;
import java.util.concurrent.TimeUnit;

import com.google.common.base.MoreObjects;
import com.google.common.base.Strings;
import com.google.common.collect.Iterables;
import com.google.common.collect.Lists;
import com.google.inject.Inject;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;
import org.eclipse.xtext.xbase.lib.Pure;
import org.eclipse.xtext.xbase.lib.util.ToStringBuilder;

import io.janusproject.services.executor.EarlyExitException;
import io.janusproject.services.executor.ExecutorService;
import io.janusproject.services.executor.JanusRunnable;

import io.sarl.core.AgentTask;
import io.sarl.core.Logging;
import io.sarl.core.Schedules;
import io.sarl.core.Time;
import io.sarl.lang.core.Agent;
import io.sarl.lang.core.AgentTrait;
import io.sarl.lang.core.Behavior;
import io.sarl.lang.core.Capacities;
import io.sarl.lang.core.SREutils;
import io.sarl.lang.core.Skill;
import io.sarl.lang.util.ClearableReference;
import io.sarl.lang.util.SynchronizedSet;
import io.sarl.util.Collections3;

/**
 * Skill that permits to execute tasks with an executor service.
 *
 * @author $Author: srodriguez$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SchedulesSkill extends BuiltinSkill implements Schedules {

	private static int installationOrder = -1;

	@Inject
	private ExecutorService executorService;

	private final Map<String, TaskDescription> tasks = new TreeMap<>();

	private ClearableReference<Skill> skillBufferLogging;

	private ClearableReference<Skill> skillBufferTime;

	/** Constructor.
	 * @param agent the owner of this skill.
	 */
	SchedulesSkill(Agent agent) {
		super(agent);
	}

	/** Replies the Logging skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final Logging getLoggingSkill() {
		if (this.skillBufferLogging == null || this.skillBufferLogging.get() == null) {
			this.skillBufferLogging = $getSkill(Logging.class);
		}
		return $castSkill(Logging.class, this.skillBufferLogging);
	}

	/** Replies the Time skill as fast as possible.
	 *
	 * @return the skill
	 */
	protected final Time getTimeSkill() {
		if (this.skillBufferTime == null || this.skillBufferTime.get() == null) {
			this.skillBufferTime = $getSkill(Time.class);
		}
		return $castSkill(Time.class, this.skillBufferTime);
	}

	/** Replies the mutex for synchronizing on the task list.
	 *
	 * @return the mutex.
	 */
	protected final Object getTaskListMutex() {
		return this.tasks;
	}

	@Override
	public int getInstallationOrder() {
		if (installationOrder < 0) {
			installationOrder = installationOrder(this);
		}
		return installationOrder;
	}

	@Override
	@Pure
	public void toString(ToStringBuilder builder) {
		super.toString(builder);
		builder.add("tasks", this.tasks); //$NON-NLS-1$
	}

	/**
	 * Remove any reference to the given task.
	 *
	 * <p>This function is not thread-safe.
	 *
	 * @param task the task.
	 * @param updateSkillReferences indicates if the references to skills should be updated too.
	 * @param updateAgentTraitReferences indicates if the references to agent traits should be updated too.
	 */
	private void finishTask(AgentTask task, boolean updateSkillReferences, boolean updateAgentTraitReferences) {
		assert task != null;
		if (updateSkillReferences) {
			this.tasks.remove(task.getName());
		}
		if (updateAgentTraitReferences) {
			final Object initiator = task.getInitiator();
			if (initiator instanceof AgentTrait) {
				final AgentTraitData data = SREutils.getSreSpecificData((AgentTrait) initiator, AgentTraitData.class);
				if (data != null) {
					data.removeTask(task);
				}
			}
		}
	}

	@Override
	public SynchronizedSet<String> getActiveTasks() {
		synchronized (getTaskListMutex()) {
			return Collections3.unmodifiableSynchronizedSet(this.tasks.keySet(), getTaskListMutex());
		}
	}

	/**
	 * Replies the names of the active futures.
	 *
	 * @return the names of the active futures.
	 */
	Collection<Future<?>> getActiveFutures() {
		synchronized (getTaskListMutex()) {
			return Lists.newArrayList(Iterables.transform(this.tasks.values(), it -> it.getFuture()));
		}
	}

	/** Unregister the tasks associated to the given behavior.
	 *
	 * @param behavior the behavior.
	 */
	synchronized void unregisterTasksForBehavior(Behavior behavior) {
		final AgentTraitData data = SREutils.getSreSpecificData(behavior, AgentTraitData.class);
		if (data != null) {
			final Iterable<AgentTask> iterable = data.resetTaskList();
			for (final AgentTask taskToCancel : iterable) {
				cancel(taskToCancel, Schedules.$DEFAULT_VALUE$CANCEL_0, false);
			}
		}
	}

	private void cancelAllRunningTasks() {
		Future<?> future;
		AgentTask task;
		for (final Entry<String, TaskDescription> taskDescription : this.tasks.entrySet()) {
			final TaskDescription pair = taskDescription.getValue();
			if (pair != null) {
				future = pair.getFuture();
				if (future != null) {
					future.cancel(true);
				}
				task = pair.getTask();
				if (task != null) {
					finishTask(task, false, true);
				}
			}
		}
		this.tasks.clear();
	}

	@Override
	protected void uninstall(UninstallationStage stage) {
		if (stage == UninstallationStage.PRE_DESTROY_EVENT) {
			// Cancel the tasks as soon as possible in the uninstallation process
			synchronized (getTaskListMutex()) {
				cancelAllRunningTasks();
			}
		} else {
			synchronized (getTaskListMutex()) {
				// Cancel the tasks that were creating during the destruction stage (in the Destroy event handler)
				cancelAllRunningTasks();
			}
		}
	}

	@Override
	public AgentTask in(long delay, Procedure1<? super Agent> procedure) {
		return in(Schedules.$DEFAULT_VALUE$IN_0, delay, procedure);
	}

	@Override
	public AgentTask in(AgentTask task, long delay, Procedure1<? super Agent> procedure) {
		TaskDescription pair;
		synchronized (getTaskListMutex()) {
			pair = preRunTask(task, procedure);
		}
		final long osDelay = Math.round(getTimeSkill().toOSDuration(delay));
		final AgentTask runnableTask = pair != null ? pair.getTask() : task;
		final ScheduledFuture<?> sf = this.executorService.schedule(
				new AgentTaskRunner(runnableTask, false),
				osDelay, TimeUnit.MILLISECONDS);
		synchronized (getTaskListMutex()) {
			pair = postRunTask(pair, task, sf);
		}
		return pair.getTask();
	}

	@Override
	public AgentTask at(AgentTask task, long time, Procedure1<? super Agent> procedure) {
		final long delay = Math.round(time - getTimeSkill().getTime());
		if (delay > 0.) {
			return in(task, delay, procedure);
		}
		return task;
	}

	private TaskDescription preRunTask(AgentTask task, Procedure1<? super Agent> procedure) {
		final TaskDescription pair;
		final AgentTask rtask;
		if (task == null) {
			pair = createTaskIfNecessary(null);
			rtask = pair.getTask();
		} else {
			rtask = task;
			pair = this.tasks.get(task.getName());
			if (pair != null) {
				pair.setTask(rtask);
			}
		}
		rtask.setProcedure(procedure);
		return pair;
	}

	private TaskDescription postRunTask(TaskDescription description, AgentTask task, Future<?> future) {
		final TaskDescription pair;
		if (description == null) {
			pair = new TaskDescription(task, future);
			this.tasks.put(task.getName(), pair);
		} else {
			pair = description;
			pair.setFuture(future);
		}
		return pair;
	}

	private TaskDescription createTaskIfNecessary(String name) {
		TaskDescription pair = null;
		final String realName;
		if (Strings.isNullOrEmpty(name)) {
			realName = "task-" + UUID.randomUUID().toString(); //$NON-NLS-1$
		} else {
			realName = name;
			synchronized (getTaskListMutex()) {
				pair = this.tasks.get(realName);
			}
		}
		if (pair == null) {
			final AgentTrait caller = Capacities.getCaller();
			final AgentTask task = new AgentTask(caller);
			task.setTaskName(realName);
			task.setGuard(AgentTask.TRUE_GUARD);
			pair = new TaskDescription(task);
			synchronized (getTaskListMutex()) {
				this.tasks.put(realName, pair);
				if (caller != null) {
					AgentTraitData data = SREutils.getSreSpecificData(caller, AgentTraitData.class);
					if (data == null) {
						data = new AgentTraitData();
						SREutils.setSreSpecificData(caller, data);
					}
					data.addTask(task);
				}
			}
		}
		return pair;
	}

	@Override
	public AgentTask task(String name) {
		return createTaskIfNecessary(name).getTask();
	}

	@Override
	public void setName(AgentTask task, String name) {
		String nm = name;
		int i = 0;
		final String prefix = name + "-"; //$NON-NLS-1$
		synchronized (getTaskListMutex()) {
			final TaskDescription desc = this.tasks.remove(task.getName());
			if (desc != null) {
				while (this.tasks.containsKey(nm)) {
					++i;
					nm = prefix + i;
				}
				task.setTaskName(nm);
				this.tasks.put(nm, desc);
			}
		}
	}

	@Override
	public final boolean cancel(AgentTask task) {
		return cancel(task, Schedules.$DEFAULT_VALUE$CANCEL_0, true);
	}

	@Override
	public final boolean cancel(AgentTask task, boolean mayInterruptIfRunning) {
		return cancel(task, mayInterruptIfRunning, true);
	}

	/** Cancel the given task with finer control on the reference updates.
	 *
	 * @param task the task to cancel.
	 * @param mayInterruptIfRunning indicates if the task's thread  could be interrupt.
	 * @param updateAgentTraitReferences indicates if the references in the task's agent trait may be updates, if
	 *     they exist.
	 * @return {@code true} if the task is canceled, {@code false} if not.
	 */
	protected boolean cancel(AgentTask task, boolean mayInterruptIfRunning, boolean updateAgentTraitReferences) {
		if (task != null) {
			final String name = task.getName();
			synchronized (getTaskListMutex()) {
				final TaskDescription pair = this.tasks.get(name);
				if (pair != null) {
					final Future<?> future = pair.getFuture();
					if (future != null && !future.isDone() && !future.isCancelled() && future.cancel(mayInterruptIfRunning)) {
						finishTask(task, true, updateAgentTraitReferences);
						return true;
					}
				}
			}
		}
		return false;
	}

	@Override
	public boolean isCanceled(AgentTask task) {
		if (task != null) {
			final String name = task.getName();
			final Future<?> future = getActiveFuture(name);
			if (future != null) {
				return future.isCancelled();
			}
		}
		return false;
	}

	/** Replies the future object for the given task.
	 *
	 * @param taskName the name of the task.
	 * @return the future.
	 * @since 0.5
	 */
	Future<?> getActiveFuture(String taskName) {
		synchronized (getTaskListMutex()) {
			final TaskDescription pair = this.tasks.get(taskName);
			if (pair != null) {
				return pair.getFuture();
			}
		}
		return null;
	}

	@Override
	public AgentTask every(long period, Procedure1<? super Agent> procedure) {
		return every(Schedules.$DEFAULT_VALUE$EVERY_0, period, procedure);
	}

	@Override
	public AgentTask every(AgentTask task, long period, Procedure1<? super Agent> procedure) {
		TaskDescription description;
		synchronized (getTaskListMutex()) {
			description = preRunTask(task, procedure);
		}
		final long osPeriod = Math.round(getTimeSkill().toOSDuration(period));
		final AgentTask runnableTask = description != null ? description.getTask() : task;
		final ScheduledFuture<?> sf = this.executorService.scheduleAtFixedRate(
				new AgentTaskRunner(runnableTask, true),
				0, osPeriod, TimeUnit.MILLISECONDS);
		synchronized (getTaskListMutex()) {
			description = postRunTask(description, task, sf);
		}
		return description.getTask();
	}

	@Override
	public AgentTask atFixedDelay(long delay, Procedure1<? super Agent> procedure) {
		return atFixedDelay(Schedules.$DEFAULT_VALUE$ATFIXEDDELAY_0, delay, procedure);
	}

	@Override
	public AgentTask atFixedDelay(AgentTask task, long delay, Procedure1<? super Agent> procedure) {
		TaskDescription description;
		synchronized (getTaskListMutex()) {
			description = preRunTask(task, procedure);
		}
		final AgentTask runnableTask = description != null ? description.getTask() : task;
		final Future<?> future;
		final long osDelay = Math.round(getTimeSkill().toOSDuration(delay));
		if (osDelay <= 0) {
			future = this.executorService.submit(new AgentInfiniteLoopTask(runnableTask));
		} else {
			future = this.executorService.scheduleWithFixedDelay(
					new AgentTaskRunner(runnableTask, true),
					0, osDelay, TimeUnit.MILLISECONDS);
		}
		synchronized (getTaskListMutex()) {
			description = postRunTask(description, task, future);
		}
		return description.getTask();
	}

	@Override
	public AgentTask execute(Procedure1<? super Agent> procedure) {
		return execute(Schedules.$DEFAULT_VALUE$EXECUTE_0, procedure);
	}

	@Override
	public synchronized AgentTask execute(AgentTask task, Procedure1<? super Agent> procedure) {
		TaskDescription description;
		synchronized (getTaskListMutex()) {
			description = preRunTask(task, procedure);
		}
		final AgentTask runnableTask = description != null ? description.getTask() : task;
		final Future<?> future = this.executorService.submit(new AgentTaskRunner(runnableTask, false));
		synchronized (getTaskListMutex()) {
			description = postRunTask(description, task, future);
		}
		return description.getTask();
	}

	/**
	 * Implementation of an agent task.
	 *
	 * @author $Author: srodriguez$
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@SuppressWarnings("synthetic-access")
	private class AgentTaskRunner extends JanusRunnable {

		private final WeakReference<AgentTask> agentTaskRef;

		private WeakReference<Future<?>> future;

		private final boolean isPeriodic;

		AgentTaskRunner(AgentTask task, boolean isPeriodic) {
			assert task != null;
			this.agentTaskRef = new WeakReference<>(task);
			this.isPeriodic = isPeriodic;
		}

		/** Set the future of this task.
		 *
		 * @param future the future.
		 * @since 0.5
		 */
		void setFuture(Future<?> future) {
			this.future = future == null ? null : new WeakReference<>(future);
		}

		/** Replies the future of this task.
		 *
		 * @return the future.
		 * @since 0.5
		 */
		private Future<?> getFuture() {
			final WeakReference<Future<?>> safeFutureReference = this.future;
			return safeFutureReference == null ? null : safeFutureReference.get();
		}

		@Override
		public void run() {
			final AgentTask task = this.agentTaskRef.get();
			if (task == null) {
				throw new RuntimeException(Messages.SchedulesSkill_0);
			}
			final Future<?> future = getFuture();
			if (future != null && (future.isDone() || future.isCancelled())) {
				setFuture(null);
				return;
			}
			boolean mustBeCanceled = false;
			try {
				final Agent owner = getOwner();
				final Function1<? super Agent, ? extends Boolean> guard = task.getGuard();
				if (guard == null || guard.apply(owner).booleanValue()) {
					final Procedure1<? super Agent> procedure = task.getProcedure();
					if (procedure != null) {
						procedure.apply(owner);
					}
				} else {
					mustBeCanceled = true;
				}
			} catch (EarlyExitException ex) {
				// Be silent
			} catch (Throwable ex) {
				getLoggingSkill().error(Messages.SchedulesSkill_1, ex, toString(), ex.getLocalizedMessage());
				mustBeCanceled = true;
			} finally {
				if (mustBeCanceled || !this.isPeriodic) {
					synchronized (getTaskListMutex()) {
						finishTask(task, true, true);
					}
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
	private class AgentInfiniteLoopTask extends JanusRunnable {
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

		private Function1<? super Agent, ? extends Boolean> getGuard() {
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
					final Function1<? super Agent, ? extends Boolean> guard = getGuard();
					if (guard == null || guard.apply(owner).booleanValue()) {
						final Procedure1<? super Agent> procedure = getProcedure();
						if (procedure != null) {
							procedure.apply(owner);
						}
					} else {
						// Break the loop without introducing a local boolean variable
						break;
					}
					Thread.yield();
				}
			} catch (EarlyExitException ex) {
				// Ignore
			} catch (Throwable ex) {
				getLoggingSkill().error(Messages.SchedulesSkill_1, ex, toString(), ex.getLocalizedMessage());
			} finally {
				final AgentTask task = this.agentTaskRef.get();
				if (task != null) {
					synchronized (getTaskListMutex()) {
						finishTask(task, true, true);
					}
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
	 * Description of a task.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	private static class TaskDescription {

		/** Agent task.
		 */
		private AgentTask task;

		/** The scheduled future associated to the task.
		 */
		private Future<?> future;

		TaskDescription(AgentTask task) {
			this.task = task;
		}

		TaskDescription(AgentTask task, Future<?> future) {
			this.task = task;
			this.future = future;
		}

		@Override
		public String toString() {
			return Objects.toString(this.task);
		}

		public AgentTask getTask() {
			return this.task;
		}

		public void setTask(AgentTask task) {
			this.task = task;
		}

		public Future<?> getFuture() {
			return this.future;
		}

		public void setFuture(Future<?> future) {
			this.future = future;
		}

	}

}
