/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

package io.sarl.lang.core;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

import io.sarl.lang.annotation.PrivateAPI;

/**
 * A possible implementation of a capacity fulfilling all the constraints of
 * this specification. Require Capacities should be accessed via the
 * {@link #getSkill(Class)} inside the {@link #install()} The Skill should
 * release all resources in the {@link #uninstall(UninstallationStage)}.
 *
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class Skill extends AgentTrait implements IBehaviorGuardEvaluatorReceiver {

	private final AtomicInteger referencesFromCapacityMap = new AtomicInteger();

	/**
	 * Creates a new Skill.
	 *
	 * <p>This constructor is provided for built-in skills, mainly.
	 *
	 * @param agent the agent that is owning the skill.
	 * @see #Skill() for the standard constructor.
	 */
	public Skill(Agent agent) {
		super(agent);
	}

	/**
	 * Creates a new Skill.
	 *
	 * <p>This constructor is provided for skills written in SARL, mainly.
	 *
	 * @see #Skill(Agent) for the built-in skill's constructor.
	 */
	public Skill() {
		super();
	}

	/** Set the agent that has this trait.
	 *
	 * @param agent the owner of this trait.
	 */
	void setOwner(Agent agent) {
		super.setOwner(agent);
		this.referencesFromCapacityMap.set(0);
	}

	/** Replies the caller of the capacity functions.
	 *
	 * <p>The replied value has a meaning inside the skills' functions that
	 * are implemented the capacities' functions.
	 *
	 * @return the caller, or {@code null} if the caller is unknown (assuming that the caller is the agent itself).
	 * @since 0.7
	 */
	@SuppressWarnings("static-method")
	@Pure
	@Inline(value = "$1.getCaller()", imported = Capacities.class, constantExpression = true)
	protected AgentTrait getCaller() {
		return Capacities.getCaller();
	}

	/**
	 * This method is called just after the installation of this skill into its
	 * owner agent. In this method you should get all information that depends
	 * on other Capacities.
	 */
	protected void install() {
		//
	}

	/**
	 * This method is called just before the "on Destroy" is fired.
	 * @since 0.11
	 */
	protected void prepareUninstallation() {
		//
	}

	/**
	 * This method is called just after the uninstallation of this skill into its
	 * owner agent.
	 * @since 0.11
	 */
	protected void uninstall() {
		//
	}

	/** Increment the reference to the skill from the capacity map.
	 *
	 * <p>This function should invokes the {@link #install()} function if it is the
	 * first call on the skill.
	 */
	void increaseReference() {
		final int oldValue = this.referencesFromCapacityMap.getAndIncrement();
		if (oldValue <= 0) {
			install();
		}
	}

	/** Decrement the reference to the skill from the capacity map.
	 *
	 * <p>This function should invokes the {@link #prepareUninstallation()} and
	 * {@link #uninstall()} functions if it is the
	 * last call on the skill.
	 */
	void decreaseReference() {
		final int newValue = this.referencesFromCapacityMap.decrementAndGet();
		if (newValue <= 0) {
			prepareUninstallation();
			uninstall();
		}
	}

	/** Replies the number of registered references to this skill into its container.
	 *
	 * @return the number of references to this skill.
	 * @since 0.11
	 */
	@PrivateAPI
	public int getReferenceCount() {
		return this.referencesFromCapacityMap.get();
	}

}
