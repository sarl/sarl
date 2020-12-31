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

package io.sarl.eclipse.launching.runner.general;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jdt.launching.VMRunnerConfiguration;

/** Implementation of a launching process.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 */
public interface ILaunchProcess {

	/** Replies the number of steps within the launching process.
	 *
	 * @return the number of steps.
	 */
	int getStepNumber();

	/** Run the next step for preparing the launching of the application.
	 *
	 * @param monitor the progress monitor.
	 * @return {@code true} if another preparation step is required. {@code false} if no more preparation step should
	 *     be run.
	 * @throws CoreException if something cannot be done.
	 */
	boolean prepare(IProgressMonitor monitor) throws CoreException;

	/** Run the next step for launching the application.
	 *
	 * @param monitor the progress monitor.
	 * @return {@code true} if another launching step is required. {@code false} if no more launching step should
	 *     be run.
	 * @throws CoreException if something cannot be done.
	 */
	boolean launch(IProgressMonitor monitor) throws CoreException;

	/** Replies the configuration of the virtual machine runner.
	 *
	 * @return the configuration of the virtual machine runner.
	 * @since 0.12
	 */
	VMRunnerConfiguration getVirtualMachineRunnerConfiguration();

}
