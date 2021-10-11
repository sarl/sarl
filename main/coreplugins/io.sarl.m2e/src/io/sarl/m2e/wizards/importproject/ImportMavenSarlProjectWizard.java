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

package io.sarl.m2e.wizards.importproject;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.Field;
import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

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

import io.sarl.m2e.SARLMavenEclipsePlugin;

/**
 * Wizard for importing a maven-based SARL project.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.8
 */
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
				final Field field = MavenImportWizard.class.getDeclaredField("page"); //$NON-NLS-1$
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
		final MavenImportWizardPage page = getMavenImportWizardPage();
		if (!page.isPageComplete()) {
			return false;
		}

		final Collection<MavenProjectInfo> projects = getProjects();

		// ignore any preselected working set
		final List<IWorkingSet> workingSets = new ArrayList<>();
		if (page.shouldCreateWorkingSet() && !projects.isEmpty()) {
			final IWorkingSet workingSet = WorkingSets.getOrCreateWorkingSet(page.getWorkingSetName());
			if (!workingSets.contains(workingSet)) {
				workingSets.add(workingSet);
			}
		}

		final WorkspaceJob importJob = createImportJob(projects);
		final WorkspaceJob globalJob;

		// XXX: The m2e plugin seems to have an issue for creating a fresh project with the SARL plugin as an extension.
		// Solution: Create a simple project, and switch to a real SARL project.
		if (!projects.isEmpty()) {
			globalJob = new WorkspaceJob("Force simple Maven project") { //$NON-NLS-1$
				@Override
				public IStatus runInWorkspace(IProgressMonitor monitor) throws CoreException {
					MultiStatus status = null;
					final SubMonitor submon = SubMonitor.convert(monitor, projects.size() * 2);
					for (final MavenProjectInfo projectInfo : projects) {
						final File pomFile = projectInfo.getPomFile();
						try {
							MavenImportUtils.forceSimplePom(pomFile.getParentFile(), monitor);
							submon.worked(1);
							// Reset the maven model
							final Model model;
							try (final InputStream inputStream = new FileInputStream(pomFile)) {
								model = MavenPlugin.getMaven().readModel(inputStream);
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
										SARLMavenEclipsePlugin.PLUGIN_ID,
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
		final WorkspaceJob job = new ImportMavenSarlProjectsJob(projects, this.workingSets, this.importConfiguration);
		job.setRule(MavenPlugin.getProjectConfigurationManager().getRule());
		return job;
	}

}
