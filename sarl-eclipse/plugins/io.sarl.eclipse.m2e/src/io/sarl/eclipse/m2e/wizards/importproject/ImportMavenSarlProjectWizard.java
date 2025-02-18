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

import java.io.FileInputStream;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import org.apache.maven.model.Model;
import org.eclipse.core.resources.WorkspaceJob;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.MultiStatus;
import org.eclipse.core.runtime.Status;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.m2e.core.MavenPlugin;
import org.eclipse.m2e.core.project.MavenProjectInfo;
import org.eclipse.m2e.core.ui.internal.WorkingSets;
import org.eclipse.m2e.core.ui.internal.wizards.MavenImportWizard;
import org.eclipse.m2e.core.ui.internal.wizards.MavenImportWizardPage;
import org.eclipse.ui.IWorkingSet;

import io.sarl.eclipse.m2e.Constants;
import io.sarl.eclipse.m2e.SARLMavenEclipsePlugin;

/**
 * Wizard for importing a maven-based SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
@SuppressWarnings("restriction")
public class ImportMavenSarlProjectWizard extends MavenImportWizard {

	private MavenImportWizardPage mainPageBuffer;

	/** Replies the main configuration page.
	 *
	 * @return the main configuration page.
	 * @throws RuntimeException a runtime exception.
	 */
	protected MavenImportWizardPage getMavenImportWizardPage() {
		if (this.mainPageBuffer == null) {
			try {
				final var field = MavenImportWizard.class.getDeclaredField("page"); //$NON-NLS-1$
				field.setAccessible(true);
				this.mainPageBuffer = (MavenImportWizardPage) field.get(this);
			} catch (Exception exception) {
				throw new RuntimeException(exception);
			}
		}
		return this.mainPageBuffer;
	}

	@Override
	public boolean performFinish() {
		final var page = getMavenImportWizardPage();
		if (!page.isPageComplete()) {
			return false;
		}

		final var projects = getProjects();

		// ignore any preselected working set
		final var workingSets = new ArrayList<IWorkingSet>();
		if (page.shouldCreateWorkingSet() && !projects.isEmpty()) {
			final var workingSet = WorkingSets.getOrCreateWorkingSet(page.getWorkingSetName());
			if (!workingSets.contains(workingSet)) {
				workingSets.add(workingSet);
			}
		}

		final var importJob = createImportJob(projects);
		final WorkspaceJob globalJob;

		// XXX: The m2e plugin seems to have an issue for creating a fresh project with the SARL plugin as an extension.
		// Solution: Create a simple project, and switch to a real SARL project.
		if (!projects.isEmpty()) {
			globalJob = new WorkspaceJob("Force simple Maven project") { //$NON-NLS-1$
				@Override
				public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
					MultiStatus status = null;
					final var submon = SubMonitor.convert(monitor, projects.size() * 2);
					for (final var projectInfo : projects) {
						final var pomFile = projectInfo.getPomFile();
						try {
							MavenImportUtils.forceSimplePom(pomFile.getParentFile(), monitor);
							submon.worked(1);
							// Reset the maven model
							final Model model;
							try (final var inputStream = new FileInputStream(pomFile)) {
								model = MavenPlugin.getMavenModelManager().readMavenModel(inputStream);
							}
							projectInfo.setModel(model);
							submon.worked(1);
						} catch (IOException | CoreException exception) {
							try {
								MavenImportUtils.restorePom(pomFile.getParentFile(), monitor);
							} catch (Exception exception0) {
								//
							}
							if (status == null) {
								status = new MultiStatus(
										Constants.PLUGIN_ID,
										IStatus.ERROR,
										exception.getLocalizedMessage(),
										exception);
							} else {
								status.add(SARLMavenEclipsePlugin.getDefault().createStatus(IStatus.ERROR, exception));
							}
						}
					}
					if (status == null) {
						importJob.schedule();
						return Status.OK_STATUS;
					}
					return status;
				}
			};
			globalJob.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
			globalJob.schedule();
		} else {
			importJob.schedule();
		}

		return true;
	}

	/** Create the import job.
	 *
	 * @param projects the projects to import.
	 * @return the import job.
	 */
	protected WorkspaceJob createImportJob(Collection<MavenProjectInfo> projects) {
		final var job = new ImportMavenSarlProjectsJob(projects, this.workingSets, this.importConfiguration);
		job.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
		return job;
	}

}
