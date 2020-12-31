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

package io.sarl.eclipse.natures;

import com.google.inject.ImplementedBy;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;

/**
 * Configurator for a SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@ImplementedBy(SARLProjectConfigurator.class)
public interface IProjectUnconfigurator {

	/** Replies if the project can be unconfigured.
	 *
	 * @param project the project.
	 * @param monitor the progress monitor.
	 * @return <code>true</code> if the project could be unconfigured.
	 */
	boolean canUnconfigure(IProject project, IProgressMonitor monitor);

	/** Unconfigure the given project.
	 *
	 * @param project the project.
	 * @param monitor the progress monitor.
	 * @throws CoreException if the project cannot be unconfigured.
	 */
	void unconfigure(IProject project, IProgressMonitor monitor) throws CoreException;

}
