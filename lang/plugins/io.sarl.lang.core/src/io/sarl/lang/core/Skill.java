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
package io.sarl.lang.core;

/**
 * A possible implementation of a capacity fulfilling all the constraints of
 * this specification. Require Capacities should be accessed via the
 * {@link #getSkill(Class)} inside the {@link #install()} The Skill should
 * release all resources in the {@link #uninstall()}.
 * 
 * @author $Author: srodriguez$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public abstract class Skill extends AgentTrait {

	/**
	 * Creates a new Skill.
	 * 
	 * @param agent
	 */
	public Skill(Agent agent) {
		super(agent);
	}

	/**
	 * This method is called just after the installation of this skill into its
	 * owner agent. In this method you should get all information that depends
	 * on other Capacities.
	 * 
	 */
	protected void install() {
		//
	}

	/**
	 * This method is called just before uninstalling the skill from its owner
	 * agent. The Skill should release all resources here.
	 * 
	 */
	protected void uninstall() {
		//
	}

}
