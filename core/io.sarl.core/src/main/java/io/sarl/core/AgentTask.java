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

import java.lang.ref.WeakReference;
import java.util.TimerTask;

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

	private TimerTask timerTask = null;
	public AgentTask() {

	}


	public Procedures.Procedure1<Agent> getProcedure() {
		return procedure;
	}


	public void setProcedure(Procedures.Procedure1<Agent> procedure) {
		this.procedure = procedure;
	}


	public Functions.Function1<Agent, Boolean> getGuard() {
		return guard;
	}

	public void setGuard(Functions.Function1<Agent, Boolean> guard) {
		this.guard = guard;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public AgentTask cancel() {
		this.timerTask.cancel();
		return this;
	}

	public AgentTask unless(Functions.Function1<Agent, Boolean> predicate) {
		this.guard = new NegateFunction(predicate);
		return this;
	}

	public AgentTask ifTrue(Functions.Function1<Agent, Boolean> predicate) {
		this.guard = predicate;
		return this;
	}

	public TimerTask getTimerTask(Agent agent) {
		this.timerTask =new AgentTimerTask(this, agent);
		return this.timerTask;
	}

	private class AgentTimerTask extends TimerTask {
		private WeakReference<AgentTask> agentTaskRef;
		private WeakReference<Agent> agentRef;

		public AgentTimerTask(AgentTask task, Agent agent) {
			this.agentTaskRef = new WeakReference<AgentTask>(task);
			this.agentRef = new WeakReference<Agent>(agent);
		}

		@Override
		public void run() {
			if (agentTaskRef.get() == null) {
				System.out.println("Agent Task is null");
			} else {
				AgentTask task = agentTaskRef.get();
				if (task.guard.apply(this.agentRef.get())) {					
					task.procedure.apply(this.agentRef.get());
				} 
			}

		}

	}

	private class NegateFunction implements Function1<Agent, Boolean> {

		private Function1<Agent, Boolean> unlessPredicate;

		public NegateFunction(Function1<Agent, Boolean> predicate) {
			this.unlessPredicate = predicate;
		}

		public Boolean apply(Agent p) {
			return !this.unlessPredicate.apply(p);
		}

	}
}
