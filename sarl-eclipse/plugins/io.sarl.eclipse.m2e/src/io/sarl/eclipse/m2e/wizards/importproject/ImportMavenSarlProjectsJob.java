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

package io.sarl.eclipse.m2e.wizards.importproject;

import java.io.IOException;
import java.lang.reflect.InvocationTargetException;
import java.util.Collection;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.m2e.core.project.MavenProjectInfo;
import org.eclipse.m2e.core.project.ProjectImportConfiguration;
import org.eclipse.m2e.core.ui.internal.Messages;
import org.eclipse.m2e.core.ui.internal.wizards.AbstractCreateMavenProjectsOperation;
import org.eclipse.m2e.core.ui.internal.wizards.ImportMavenProjectsJob;
import org.eclipse.m2e.core.ui.internal.wizards.MappingDiscoveryJob;
import org.eclipse.m2e.core.ui.internal.wizards.MavenProjectWorkspaceAssigner;
import org.eclipse.ui.IWorkingSet;

/**
 * Workspace Job for importing {@link MavenProjectInfo}s into the workspace, and fixing the
 * issues related to the import of "extensions"-true plugins. After the projects are imported, if
 * lifecycle mappings errors have been detected on the imported projects, the Lifecycle Mapping wizard is shown to help
 * users fix these errors.
 *
 * <p>This class is a copy/paster of {@link ImportMavenProjectsJob}. And, it is fixed for Maven SARL projects.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse.m2e 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.m2e
 * @since 0.8
 * @see ImportMavenProjectsJob
 */
@SuppressWarnings("restriction")
class ImportMavenSarlProjectsJob extends WorkspaceJob {

	private final List<IWorkingSet> workingSets;

	private final Collection<MavenProjectInfo> projects;

	private final ProjectImportConfiguration importConfiguration;

	/** Constructor.
	 *
	 * @param projects the projects to import.
	 * @param workingSets the working sets into which the projects should be added.
	 * @param importConfiguration the import configuration.
	 */
	ImportMavenSarlProjectsJob(
			Collection<MavenProjectInfo> projects,
			List<IWorkingSet> workingSets,
			ProjectImportConfiguration importConfiguration) {
		super(Messages.MavenImportWizard_job);
		this.projects = projects;
		this.workingSets = workingSets;
		this.importConfiguration = importConfiguration;
	}

	/** Replies the projects to import.
	 *
	 * @return the projects to import.
	 */
	protected Collection<MavenProjectInfo> getProjects() {
		return this.projects;
	}

	/** Replies the import configuration.
	 *
	 * @return the import configuration.
	 */
	protected ProjectImportConfiguration getImportConfiguration() {
		return this.importConfiguration;
	}

	/** Replies the workings into which the projects should be added.
	 *
	 * @return the working sets.
	 */
	protected List<IWorkingSet> getWorkingSets() {
		return this.workingSets;
	}

	/** Create the operation for creating the projects.
	 *
	 * @return the operation.
	 */
	protected AbstractCreateMavenProjectsOperation createOperation() {
		return new AbstractCreateMavenProjectsOperation() {
			@Override
			protected List<IProject> doCreateMavenProjects(IProgressMonitor progressMonitor) throws CoreException {
				final var monitor = SubMonitor.convert(progressMonitor, 101);
				try {
					final var results = MavenImportUtils.runFixedImportJob(
							true,
							getProjects(),
							getImportConfiguration(),
							new MavenProjectWorkspaceAssigner(getWorkingSets()),
							monitor.newChild(100));
					return toProjects(results);
				} finally {
					restorePom();
					monitor.done();
				}
			}
		};
	}

	private void restorePom() {
		for (final MavenProjectInfo project : getProjects()) {
			try {
				MavenImportUtils.restorePom(project.getPomFile().getParentFile(), new NullProgressMonitor());
			} catch (IOException exception) {
				//
			}
		}
	}

	@Override
	public IStatus runInWorkspace(final IProgressMonitor monitor) throws CoreException {
		final AbstractCreateMavenProjectsOperation importOperation = createOperation();
		try {
			importOperation.run(monitor);
			final var createdProjects = importOperation.getCreatedProjects();
			final var discoveryJob = new MappingDiscoveryJob(createdProjects, false);
			discoveryJob.schedule();
		} catch (InvocationTargetException exception) {
			restorePom();
			return AbstractCreateMavenProjectsOperation.toStatus(exception);
		}
		return Status.OK_STATUS;
	}

}
