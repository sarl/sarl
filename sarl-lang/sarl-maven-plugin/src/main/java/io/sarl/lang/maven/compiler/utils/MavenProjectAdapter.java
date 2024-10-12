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

package io.sarl.lang.maven.compiler.utils;

import org.apache.maven.project.MavenProject;
import org.eclipse.emf.common.notify.impl.AdapterImpl;
import org.eclipse.emf.ecore.resource.ResourceSet;

/** Adapter for linking Ecore objects to the Maven project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public final class MavenProjectAdapter extends AdapterImpl {

	private final MavenProject project;

	private MavenProjectAdapter(MavenProject project) {
		this.project = project;
	}

	/** Replies the Maven project for the resource set.
	 *
	 * @param rs the manve project.
	 * @return the maven project.
	 */
	public static MavenProject get(ResourceSet rs) {
		for (final var a : rs.eAdapters()) {
			if (a instanceof MavenProjectAdapter cvalue) {
				return cvalue.project;
			}
		}
		throw new RuntimeException(Messages.MavenProjectAdapter_0);
	}

	/** Install the adapter.
	 *
	 * @param rs the resource set that will receive the project.
	 * @param project the project.
	 */
	public static void install(ResourceSet rs, MavenProject project) {
		final var iterator = rs.eAdapters().iterator();
		while (iterator.hasNext()) {
			if (iterator.next() instanceof MavenProjectAdapter) {
				iterator.remove();
			}
		}
		rs.eAdapters().add(new MavenProjectAdapter(project));
	}

}
