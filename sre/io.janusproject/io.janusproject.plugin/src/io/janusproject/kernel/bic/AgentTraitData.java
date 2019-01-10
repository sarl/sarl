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
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Objects;

import com.google.common.collect.Iterables;

import io.sarl.core.AgentTask;

/**
 * Data associated to an agent trait.
 *
 * <p>This object is not thread-safe.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.5
 */
class AgentTraitData {

	private List<WeakReference<AgentTask>> tasks = new ArrayList<>();

	/** Add a task.
	 *
	 * @param task the task.
	 */
	public void addTask(AgentTask task) {
		this.tasks.add(new WeakReference<>(task));
	}

	@Override
	public String toString() {
		return Iterables.toString(getTaskList(this.tasks));
	}

	/** Remove task.
	 *
	 * @param task the task.
	 */
	public void removeTask(AgentTask task) {
		final Iterator<WeakReference<AgentTask>> iterator = this.tasks.iterator();
		while (iterator.hasNext()) {
			final WeakReference<AgentTask> reference = iterator.next();
			final AgentTask knownTask = reference.get();
			if (knownTask == null) {
				iterator.remove();
			} else if (Objects.equals(knownTask.getName(), task.getName())) {
				iterator.remove();
				return;
			}
		}
	}

	/** Reset the task list.
	 *
	 * @return the old task list.
	 */
	public Iterable<AgentTask> resetTaskList() {
		final List<WeakReference<AgentTask>> old = this.tasks;
		this.tasks = new ArrayList<>();
		return getTaskList(old);
	}

	/** Reset the task list.
	 *
	 * @param tasks the original tasks.
	 * @return the task list.
	 */
	protected static Iterable<AgentTask> getTaskList(List<WeakReference<AgentTask>> tasks) {
		final Iterable<AgentTask> col = Iterables.transform(tasks, it -> it != null ? it.get() : null);
		return Iterables.filter(col, it -> it != null);
	}

}
