/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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

import org.eclipse.core.runtime.CoreException;
import org.eclipse.xtext.builder.JDTAwareEclipseResourceFileSystemAccess2;
import org.eclipse.xtext.generator.OutputConfiguration;
import org.eclipse.xtext.util.RuntimeIOException;

/** A manager of file system accesses that is JDT aware.
 *
 * <p>This implementation fix an issue in the xtext manager that is not adding
 * the source folder when ensuring the file system folder is existing.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class ProjectRelativeFileSystemAccess extends JDTAwareEclipseResourceFileSystemAccess2 {

	@Override
	protected boolean ensureOutputConfigurationDirectoryExists(OutputConfiguration outputConfig) {
		try {
			if (super.ensureOutputConfigurationDirectoryExists(outputConfig)) {
				final var container = getContainer(outputConfig);
				addToSourceFolders(container);
				return true;
			}
			return false;
		} catch (CoreException e) {
			throw new RuntimeIOException(e);
		}
	}

}

