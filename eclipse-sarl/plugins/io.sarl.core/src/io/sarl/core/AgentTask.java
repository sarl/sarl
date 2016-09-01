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

package io.sarl.core;

import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures;

import io.sarl.lang.core.Agent;

/**
 * Description of a task an agent has schedule to be performed at a later time.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AgentTask {

	private static final Function1<Agent, Boolean> FALSE_GUARD = (agent) -> Boolean.FALSE;

	private String name;

	private Function1<Agent, Boolean> guard;

	private Procedures.Procedure1<? super Agent> procedure;

	/** Construct an AgentTask.
	 */
	public AgentTask() {
		//
	}

	/** Replies the procedure that is associated to this task.
	 *
	 * @return the procedure to run.
	 */
	public Procedures.Procedure1<? super Agent> getProcedure() {
		return this.procedure;
	}

	/** Change the procedure associated to this task.
	 *
	 * @param procedure - code of the procedure to associate to the task.
	 */
	public void setProcedure(Procedures.Procedure1<? super Agent> procedure) {
		this.procedure = procedure;
	}

	/** Replies the guard of this task.
	 *
	 * @return the guard.
	 */
	public Function1<Agent, Boolean> getGuard() {
		return this.guard;
	}

	/** Change the guard of this task.
	 *
	 * @param guard - the code of the function that is the predicate associated to the guard.
	 * @see #unless
	 * @see #ifTrue
	 */
	public void setGuard(Function1<Agent, Boolean> guard) {
		this.guard = guard;
	}

	/** Replies the name of this task.
	 *
	 * @return the name.
	 */
	public String getName() {
		return this.name;
	}

	/** Change the name of this task.
	 *
	 * @param name - name of the task.
	 */
	public void setName(String name) {
		this.name = name;
	}

	/** Change the gard of this that with the negation of the given predicate.
	 *
	 * @param predicate - the code of the function that is the predicate to test.
	 * @return <code>this</code>.
	 * @see #setGuard
	 */
	public AgentTask unless(final Function1<Agent, Boolean> predicate) {
		if (predicate == null) {
			this.guard = FALSE_GUARD;
		} else {
			this.guard = (agent) -> predicate.apply(agent) == Boolean.TRUE ? Boolean.FALSE : Boolean.TRUE;
		}
		return this;
	}

	/** Change the gard to the given predicate.
	 *
	 * @param predicate - the code of the function that is the predicate to test.
	 * @return <code>this</code>.
	 * @see #setGuard
	 */
	public AgentTask ifTrue(Function1<Agent, Boolean> predicate) {
		if (predicate == null) {
			this.guard = null;
		} else {
			this.guard = predicate;
		}
		return this;
	}

	@Override
	public String toString() {
		return "AgentTask: " + this.name; //$NON-NLS-1$
	}

}
