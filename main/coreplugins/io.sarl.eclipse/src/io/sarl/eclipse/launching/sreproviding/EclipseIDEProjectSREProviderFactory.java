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

package io.sarl.eclipse.launching.sreproviding;

import org.eclipse.core.resources.IProject;

import io.sarl.eclipse.runtime.ProjectSREProvider;
import io.sarl.eclipse.runtime.ProjectSREProviderFactory;

/** Factory of the default implementation of a project SRE provider.
 * This provider is reading the Eclipse IDE properties associated to the project and
 * determine the corresponding SRE.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see EclipseIDEProjectSREProvider
 */
public class EclipseIDEProjectSREProviderFactory implements ProjectSREProviderFactory {

	/** Construct a factory of SRE provider.
	 */
	public EclipseIDEProjectSREProviderFactory() {
		//
	}

	@Override
	public ProjectSREProvider getProjectSREProvider(IProject project) {
		return new EclipseIDEProjectSREProvider(project);
	}

}
