/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.examples.wizard;

import java.io.File;
import java.lang.ref.WeakReference;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.SubMonitor;
import org.eclipse.debug.internal.ui.SWTFactory;
import org.eclipse.emf.common.ui.wizard.ExampleInstallerWizard;
import org.eclipse.jface.wizard.WizardPage;
import org.eclipse.swt.SWT;
import org.eclipse.swt.layout.GridLayout;
import org.eclipse.swt.widgets.Button;
import org.eclipse.swt.widgets.Composite;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.intro.IIntroManager;
import org.eclipse.ui.intro.IIntroPart;
import org.eclipse.ui.wizards.datatransfer.ImportOperation;

import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.lang.SARLConfig;
import io.sarl.m2e.wizards.importproject.MavenImportUtils;

/** Wizard for importing SARL samples.
 *
 * <p>This wizard extends the EMF wizard with the initialization of the SARL nature on the project,
 * the creation of launch configurations, and the closing of the welcome page of Eclipse.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.6
 */
public class SarlExampleInstallerWizard extends ExampleInstallerWizard {

	private ConfigurationPage configurationPage;

	@Override
	public void addPages() {
		// Override the default "addPages" in order to create a configuration page.
		super.addPages();
		this.configurationPage = new ConfigurationPage("configurationPage", Messages.SarlExampleInstallerWizard_0); //$NON-NLS-1$
		this.configurationPage.setDescription(Messages.SarlExampleInstallerWizard_1);
		addPage(this.configurationPage);
	}

	@Override
	public void dispose() {
		this.configurationPage.dispose();
		this.configurationPage = null;
		super.dispose();
	}

	private void fixFilesToOpen(IProject project) {
		if (this.filesToOpen != null) {
			final List<FileToOpen> fixedList = new ArrayList<>();
			for (final FileToOpen file : this.filesToOpen) {
				if (!(file instanceof ProjectFileToOpen)) {
					final ProjectFileToOpen projectFile = new ProjectFileToOpen(project, file);
					fixedList.add(projectFile);
				} else {
					fixedList.add(file);
				}
			}
			this.filesToOpen = fixedList;
		}
	}

	@Override
	protected void installProject(ProjectDescriptor projectDescriptor, IProgressMonitor progressMonitor)
			throws Exception {
		final SubMonitor mon = SubMonitor.convert(progressMonitor, 2);
		super.installProject(projectDescriptor, mon.newChild(1));
		postProjectInstallation(projectDescriptor, mon.newChild(1));
	}

	/** Post process the given project.
	 *
	 * @param projectDescriptor the descriptor of the project.
	 * @param progressMonitor the progression monitor.
	 * @throws Exception if the post process cannot be applied.
	 */
	protected void postProjectInstallation(ProjectDescriptor projectDescriptor, IProgressMonitor progressMonitor) throws Exception {
		final SubMonitor mon = SubMonitor.convert(progressMonitor, 3);

		// Force the natures of the project
		final IProject project = projectDescriptor.getProject();

		final IFile pomFile = project.getFile(Path.fromOSString("pom.xml")); //$NON-NLS-1$
		if (this.configurationPage.isMavenNatureEnabled() && pomFile.exists()) {
			// The project should be a Maven project.
			final IPath descriptionFilename = project.getFile(new Path(IProjectDescription.DESCRIPTION_FILE_NAME)).getLocation();
			final File projectDescriptionFile = descriptionFilename.toFile();
			// Project was open by the super class. Close it because Maven fails when a project already exists.
			project.close(mon.newChild(1));
			// Delete the Eclipse project definition because Maven fails when a project already exists.
			project.delete(false, true, mon.newChild(1));
			if (projectDescriptionFile.exists()) {
				projectDescriptionFile.delete();
			}
			// Import
			MavenImportUtils.importMavenProject(
					project.getWorkspace().getRoot(),
					projectDescriptor.getName(),
					true,
					mon.newChild(1));
		} else {
			// Force the project configuration to SARL.
			SARLProjectConfigurator.configureSARLProject(
					// Project to configure
					project,
					// Add SARL natures
					true,
					// Force java configuration
					true,
					// Create folders
					true,
					// Monitor
					mon.newChild(1));
		}

		// Fixing the names of the files to open because the project's names were not originally specified
		fixFilesToOpen(project);

		mon.done();
	}

	@Override
	public boolean performFinish() {
		if (super.performFinish()) {
			// Close the welcome page.
			closeWelcomePage();
			return true;
		}
		if (this.configurationPage != null && !this.configurationPage.getControl().isDisposed()) {
			this.configurationPage.refresh();
		}
		return false;
	}

	/** Close the welcome page.
	 */
	protected static void closeWelcomePage() {
		final IIntroManager introManager = PlatformUI.getWorkbench().getIntroManager();
		if (introManager != null) {
			final IIntroPart intro = introManager.getIntro();
			if (intro != null) {
				introManager.closeIntro(intro);
			}
		}
	}

	/** Configuration page.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.8
	 */
	public class ConfigurationPage extends WizardPage {

		/** Indicates if the maven nature must be enabled by default.
		 */
		public static final boolean DEFAULT_MAVEN_NATURE = true;

		private Button mavenProjectButton;

		/** Constructor.
		 *
		 * @param pageName the name of the page.
		 * @param title the title.
		 */
		public ConfigurationPage(String pageName, String title) {
			super(pageName, title, null);
		}

		@Override
		public void createControl(Composite parent) {
			final Composite composite = new Composite(parent, SWT.NONE);
			final GridLayout layout = new GridLayout(2, false);
			final int margin = -5;
			final int spacing = 3;
			layout.marginTop = margin;
			layout.marginLeft = margin;
			layout.marginRight = margin;
			layout.marginBottom = margin;
			layout.horizontalSpacing = spacing;
			layout.verticalSpacing = spacing;
			composite.setLayout(layout);

			this.mavenProjectButton = SWTFactory.createCheckButton(composite,
					Messages.SarlExampleInstallerWizard_2, null, DEFAULT_MAVEN_NATURE, 2);

			refresh();
			setControl(composite);
		}

		/** Refresh the configuration page.
		 */
		public void refresh() {
			setErrorMessage(null);
			setPageComplete(true);
		}

		/** Replies if the maven nature must be enabled.
		 *
		 * @return {@code true} for enabling the maven nature.
		 */
		public boolean isMavenNatureEnabled() {
			return this.mavenProjectButton.getSelection();
		}


	}


	/** A file to be opened that is associated to a project.
	 *
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.10
	 */
	public static class ProjectFileToOpen extends FileToOpen {

		private WeakReference<IProject> project;

		private IFile projectFile;

		/** Constructor.
		 *
		 * @param project the container of the file.
		 * @param source the source description.
		 */
		public ProjectFileToOpen(IProject project, FileToOpen source) {
			this.project = new WeakReference<>(project);
			setLocation(source.getLocation());
			setEditorID(source.getEditorID());
		}

		/** Replies the file into the project.
		 *
		 * @return the file or {@code null} if the file cannot be found.
		 */
		@SuppressWarnings("checkstyle:nestedifdepth")
		public IFile findProjectFile() {
			if (this.projectFile == null) {
				final IProject prj = this.project.get();
				if (prj != null) {
					// 3 cases for location:
					// * prj / src folder / source path
					// * src folder / source path
					// * source path
					final IPath path0 = Path.fromPortableString(getLocation());
					IFile file = ResourcesPlugin.getWorkspace().getRoot().getFile(path0);
					if (file != null && file.exists()) {
						this.projectFile = file;
					} else {
						file = prj.getFile(path0);
						if (file != null && file.exists()) {
							this.projectFile = file;
						} else {
							final IPath srcFolder = Path.fromOSString(SARLConfig.FOLDER_SOURCE_SARL);
							final IPath path1 = srcFolder.append(path0);
							file = prj.getFile(path1);
							if (file != null && file.exists()) {
								this.projectFile = file;
							}
						}
					}
				}
			}
			return this.projectFile;
		}

		@Override
		public IFile getWorkspaceFile() {
			if (this.workspaceFile == null) {
				final IFile projectFile = findProjectFile();
				if (projectFile != null) {
					this.workspaceFile = projectFile;
				} else {
					return super.getWorkspaceFile();
				}
			}
			return this.workspaceFile;
		}

	}

}
