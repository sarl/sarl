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

package io.sarl.eclipse.launching.runner.general;

/** Steps of preparation in the launching process.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
public enum PreparationProcessState {
	/** Get launch configuration parameters.
	 */
	STEP_0_PREPARE_PARAMETERS,

	/** Build the run class path.
	 */
	STEP_1_BUILD_CLASSPATH,

	/** Prepare launching.
	 */
	STEP_2_PREPARE_LAUNCHING,

	/** Validate launching arguments.
	 */
	STEP_3_POST_VALIDATION,

	/** Create the concrete run configuration.
	 */
	STEP_4_CREATE_RUN_CONFIGURATION,

	/** Configure "stop in main".
	 */
	STEP_5_CONFIGURE_STOP_IN_MAIN;

	/** Replies the next step.
	 *
	 * @return the next step.
	 */
	public PreparationProcessState next() {
		final var index = ordinal() + 1;
		final var vals = values();
		if (index < vals.length) {
			return vals[index];
		}
		return this;
	}

}
