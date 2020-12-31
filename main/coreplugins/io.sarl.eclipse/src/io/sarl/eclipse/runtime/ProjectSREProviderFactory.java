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

package io.sarl.eclipse.runtime;

import org.eclipse.core.resources.IProject;

/** The objects that are implementing this interface are able to
 * create instances of {@link ProjectSREProvider}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public interface ProjectSREProviderFactory {

	/** Replies a project SRE provider that may be
	 * used for a specific purpose.
	 *
	 * @param project the project for which the provider must be created.
	 * @return the project SRE provider.
	 */
	ProjectSREProvider getProjectSREProvider(IProject project);

}
