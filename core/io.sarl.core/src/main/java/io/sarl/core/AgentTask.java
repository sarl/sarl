/*
 * Copyright 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.core;

import io.sarl.lang.core.Agent;

import org.eclipse.xtext.xbase.lib.Functions;
import org.eclipse.xtext.xbase.lib.Functions.Function1;
import org.eclipse.xtext.xbase.lib.Procedures;

/**
 * Description of a task an agent has schedule to be performed at a later time.
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class AgentTask {

	private String name;
	private Functions.Function1<Agent, Boolean> guard;
	private Procedures.Procedure1<Agent> procedure;
	
	/**
	 */
	public AgentTask() {
		//
	}

	/** Replies the procedure that is associated to this task.
	 * 
	 * @return the procedure to run.
	 */
	public Procedures.Procedure1<Agent> getProcedure() {
		return this.procedure;
	}

	/** Change the procedure associated to this task.
	 * 
	 * @param procedure
	 */
	public void setProcedure(Procedures.Procedure1<Agent> procedure) {
		this.procedure = procedure;
	}

	/** Replies the guard of this task.
	 * 
	 * @return the guard.
	 */
	public Functions.Function1<Agent, Boolean> getGuard() {
		return this.guard;
	}

	/** Change the guard of this task.
	 * 
	 * @param guard
	 * @see #unless(Function1)
	 * @see #ifTrue(Function1)
	 */
	public void setGuard(Functions.Function1<Agent, Boolean> guard) {
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
	 * @param name
	 */
	public void setName(String name) {
		this.name = name;
	}

	/** Change the gard of this that with the negation of the given predicate.
	 * 
	 * @param predicate
	 * @return <code>this</code>.
	 * @see #setGuard(Function1)
	 */
	public AgentTask unless(Functions.Function1<Agent, Boolean> predicate) {
		this.guard = new NegateFunction(predicate);
		return this;
	}

	/** Change the gard to the given predicate.
	 * 
	 * @param predicate
	 * @return <code>this</code>.
	 * @see #setGuard(Function1)
	 */
	public AgentTask ifTrue(Functions.Function1<Agent, Boolean> predicate) {
		this.guard = predicate;
		return this;
	}

	/**
	 * @author $Author: srodriguez$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	private class NegateFunction implements Function1<Agent, Boolean> {

		private Function1<Agent, Boolean> unlessPredicate;

		public NegateFunction(Function1<Agent, Boolean> predicate) {
			this.unlessPredicate = predicate;
		}

		@Override
		public Boolean apply(Agent p) {
			return !this.unlessPredicate.apply(p);
		}

	}
}
