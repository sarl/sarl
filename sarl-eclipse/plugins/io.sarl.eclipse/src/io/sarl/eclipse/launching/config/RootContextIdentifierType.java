/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.launching.config;

/**
 * Type of the root context identifier in the multiagent system.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public enum RootContextIdentifierType {

	/** The identifier of the root context is the one
	 * given by the SRE.
	 */
	DEFAULT_CONTEXT_ID,

	/** The identifier of the root context is randomly
	 * computed at each start of the SRE.
	 */
	RANDOM_CONTEXT_ID,

	/** The identifier of the root context is computed
	 * from the name of the type of the bott agent.
	 */
	BOOT_AGENT_CONTEXT_ID;

}
