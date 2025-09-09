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

package io.sarl.apputils.uiextensions;

import java.util.Arrays;
import java.util.LinkedList;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.SubMonitor;

/**
 * Tools related to the Eclipse project natures.
 *
 * @author $Author: sgalland$
 * @version uiextensions 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid uiextensions
 * @since 0.15
 */
public final class ProjectNatures {

	/** Constructor.
	 */
	private ProjectNatures() {
		//
	}

	/** Add the natures to the given project.
	 *
	 * @param project the project.
	 * @param monitor the monitor.
	 * @param natureIdentifiers the identifiers of the natures to add to the project.
	 * @return the status if the operation.
	 */
	public static IStatus addNatures(IProject project, IProgressMonitor monitor, String... natureIdentifiers) {
		if (project != null && natureIdentifiers != null && natureIdentifiers.length > 0) {
			try {
				final var subMonitor = SubMonitor.convert(monitor, natureIdentifiers.length + 2);
				final var description = project.getDescription();
				final var natures = new LinkedList<>(Arrays.asList(description.getNatureIds()));

				for (final var natureIdentifier : natureIdentifiers) {
					if (natureIdentifier != null && natureIdentifier.length() > 0 && !natures.contains(natureIdentifier)) {
						natures.add(0, natureIdentifier);
					}
					subMonitor.worked(1);
				}

				final var newNatures = natures.toArray(new String[natures.size()]);
				final var status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);
				subMonitor.worked(1);

				// check the status and decide what to do
				if (status.getCode() == IStatus.OK) {
					description.setNatureIds(newNatures);
					project.setDescription(description, subMonitor.newChild(1));
				}
				subMonitor.done();
				return status;
			} catch (CoreException exception) {
				return UiExtensionsPlugin.getDefault().createStatus(IStatus.ERROR, exception);
			}
		}
		return UiExtensionsPlugin.getDefault().createOkStatus();
	}

}
