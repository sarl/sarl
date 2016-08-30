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

package io.sarl.docs.tutorials.tasks;

/** State of the task manager.
 *
 * @author $Author: galland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public enum TaskManagerState {
	/** No execution request. */
	NO_REQUEST,
	/** Waiting for end of execution with a successful execution. */
	WAITING_EXECUTION_TERMINATION_WITH_SUCCESS,
	/** Waiting for end of execution with a errorneous execution. */
	WAITING_EXECUTION_TERMINATION_WITH_ERROR,
	/** Execution is terminated on success. */
	SUCCESS,
	/** Execution is terminated on error. */
	ERROR
}
