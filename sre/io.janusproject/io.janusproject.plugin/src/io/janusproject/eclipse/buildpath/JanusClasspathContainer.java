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

package io.janusproject.eclipse.buildpath;

import java.util.Arrays;
import java.util.List;
import java.util.Set;

import org.eclipse.core.runtime.IPath;
import org.eclipse.jdt.core.IClasspathEntry;

import io.sarl.eclipse.buildpath.SARLClasspathContainer;

/** Classpath container dedicated to the Janus platform.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class JanusClasspathContainer extends SARLClasspathContainer {

	/** Names of the reference libraries that are required to compile the Janus
	 * code and the generated Java code.
	 */
	public static final String[] JANUS_REFERENCE_LIBRARIES = {
		"org.arakhne.afc.core.vmutils", //$NON-NLS-1$
		"org.arakhne.afc.core.util", //$NON-NLS-1$
		"com.hazelcast", //$NON-NLS-1$
		"com.google.gson", //$NON-NLS-1$
		"org.zeromq.jeromq", //$NON-NLS-1$
		"com.google.inject", //$NON-NLS-1$
		"org.apache.commons.cli", //$NON-NLS-1$
		"io.janusproject.plugin", //$NON-NLS-1$
	};

	/** Constructor.
	 *
	 * @param containerPath - the path of the container, e.g. the project.
	 */
	public JanusClasspathContainer(IPath containerPath) {
		super(containerPath);
	}

	@Override
	public Set<String> getBundleDependencies() {
		final Set<String> deps = super.getBundleDependencies();
		deps.addAll(Arrays.asList(JANUS_REFERENCE_LIBRARIES));
		return deps;
	}

	@Override
	protected void updateEntries(List<IClasspathEntry> entries) throws Exception {
		// Add the SARL dependencies.
		super.updateEntries(entries);
		// Add the Janus dependencies.
		for (final String referenceLibrary : JANUS_REFERENCE_LIBRARIES) {
			entries.add(newLibrary(referenceLibrary));
		}
	}

	@Override
	public String getDescription() {
		return Messages.JanusClasspathContainer_0;
	}

}
