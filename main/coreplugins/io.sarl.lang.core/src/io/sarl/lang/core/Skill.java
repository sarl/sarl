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

package io.sarl.lang.core;

import java.util.concurrent.atomic.AtomicInteger;

import org.eclipse.xtext.xbase.lib.Inline;
import org.eclipse.xtext.xbase.lib.Pure;

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
public abstract class Skill extends AgentTrait {

	private final AtomicInteger uses = new AtomicInteger();

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

	/** Mark this skill as used by one user.
	 */
	void registerUse() {
		final int value = this.uses.getAndIncrement();
		if (value == 0) {
			install();
		}
	}

	/** Mark this skill as release by one user.
	 */
	void unregisterUse() {
		final int value = this.uses.decrementAndGet();
		if (value == 0) {
			uninstall(UninstallationStage.PRE_DESTROY_EVENT);
			uninstall(UninstallationStage.POST_DESTROY_EVENT);
		}
	}

	/**
	 * This method is called just after the installation of this skill into its
	 * owner agent. In this method you should get all information that depends
	 * on other Capacities.
	 */
	protected void install() {
		//
	}

	/** This method is called just before uninstalling the skill from its owner agent.
	 * The Skill should release all resources here.
	 *
	 * @deprecated see {@link #uninstall(UninstallationStage)} with {@link UninstallationStage#POST_DESTROY_EVENT} argument.
	 */
	@Deprecated
	protected void uninstall() {
		//
	}

	/**
	 * This method is called just before uninstalling the skill from its owner agent.
	 *
	 * @param stage indicates the stage in the uninstallation process.
	 * @since 0.5
	 */
	protected void uninstall(UninstallationStage stage) {
		// This following code should be removed when uninstalled() is removed.
		if (stage == UninstallationStage.POST_DESTROY_EVENT) {
			uninstall();
		}
	}

	/** Sage in the skill uninstallation process.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.5
	 */
	public enum UninstallationStage {

		/** The stage is before the event handlers for {@code Destroy} are invoked.
		 *
		 * <p>During this stage, the skill could release resources before the destruction functions of its agent are invoked.
		 */
		PRE_DESTROY_EVENT,

		/** The stage is after the event handlers for {@code Destroy} are invoked.
		 *
		 * <p>During this stage, the skill should release all the resources that are still used by the skill.
		 */
		POST_DESTROY_EVENT,

	}

}
