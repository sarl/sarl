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

package io.sarl.lang.ui.compiler;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.Path;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.util.EcoreUtil;

/** Adapter for retreiving a project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class ProjectAdapter extends AdapterImpl {

	private final IProject project;

	/** Constructor.
	 *
	 * @param project the project.
	 */
	public ProjectAdapter(IProject project) {
		this.project = project;
	}

	@Override
	public boolean isAdapterForType(Object type) {
		return ProjectAdapter.class.equals(type);
	}

	/** Replies the project.
	 *
	 * @return the project.
	 */
	public IProject getProject() {
		return this.project;
	}

	/** Get the project associated to the resource.
	 *
	 * @param resource the resource
	 * @return the project.
	 */
	public static IProject getProject(Resource resource) {
		ProjectAdapter adapter = (ProjectAdapter) EcoreUtil.getAdapter(resource.getResourceSet().eAdapters(), ProjectAdapter.class);
		if (adapter == null) {
			final String platformString = resource.getURI().toPlatformString(true);
			final IProject project = ResourcesPlugin.getWorkspace().getRoot().getFile(new Path(platformString)).getProject();
			adapter = new ProjectAdapter(project);
			resource.getResourceSet().eAdapters().add(adapter);
		}
		return adapter.getProject();
	}

}
