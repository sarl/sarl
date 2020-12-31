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

package io.sarl.maven.compiler;

import javax.inject.Provider;

import org.apache.maven.project.MavenProject;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.XtextResourceSet;

/** provider of resource sets when comilig with Maven.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
class MavenProjectResourceSetProvider implements Provider<ResourceSet> {

	private final MavenProject project;

	private ResourceSet resourceSet;

	/** Constructor.
	 *
	 * @param project the compiled project.
	 */
	MavenProjectResourceSetProvider(MavenProject project) {
		super();
		assert project != null;
		this.project = project;
	}

	@Override
	public ResourceSet get() {
		ResourceSet rs = this.resourceSet;
		if (rs == null) {
			rs = new XtextResourceSet();
			MavenProjectAdapter.install(rs, this.project);
			this.resourceSet = rs;
		}
		assert rs != null;
		return rs;
	}

}
