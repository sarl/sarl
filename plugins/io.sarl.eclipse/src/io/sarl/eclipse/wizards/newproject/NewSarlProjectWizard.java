/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2015 the original authors or authors.
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

package io.sarl.eclipse.wizards.newproject;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.QualifiedName;
import org.eclipse.jdt.core.IClasspathEntry;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.packageview.PackageExplorerPart;
import org.eclipse.jdt.internal.ui.util.ExceptionHandler;
import org.eclipse.jdt.internal.ui.wizards.JavaProjectWizard;
import org.eclipse.jdt.internal.ui.wizards.NewElementWizard;
import org.eclipse.jdt.internal.ui.wizards.NewWizardMessages;
import org.eclipse.jdt.ui.IPackagesViewPart;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.IWorkbenchWindow;
import org.eclipse.ui.IWorkingSet;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;

import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.properties.RuntimeEnvironmentPropertyPage;
import io.sarl.eclipse.runtime.ISREInstall;
import io.sarl.eclipse.util.Utilities;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.ui.preferences.SARLPreferences;

/**
 * SARL new project wizard.
 * Most part of the code of this class comes from {@link JavaProjectWizard}.
 *
 * @author $Author: ngaud$
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class NewSarlProjectWizard extends NewElementWizard implements IExecutableExtension {

	private MainProjectWizardPage fFirstPage;

	private BuildSettingWizardPage fSecondPage;

	private IConfigurationElement fConfigElement;

	/** Construct a new wizard for creating a SARL project.
	 */
	public NewSarlProjectWizard() {
		this(null, null);
	}


	/** Construct a new wizard for creating a SARL project.
	 *
	 * @param pageOne - reference to the first page of the wizard.
	 * @param pageTwo - reference to the second page of the wizard.
	 */
	public NewSarlProjectWizard(MainProjectWizardPage pageOne, BuildSettingWizardPage pageTwo) {
		setDefaultPageImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		setWindowTitle(Messages.SARLProjectNewWizard_0);

		this.fFirstPage = pageOne;
		this.fSecondPage = pageTwo;
	}

	@Override
	public void addPages() {
		if (this.fFirstPage == null) {
			this.fFirstPage = new MainProjectWizardPage();
		}
		addPage(this.fFirstPage);

		if (this.fSecondPage == null) {
			this.fSecondPage = new BuildSettingWizardPage(this.fFirstPage);
		}
		addPage(this.fSecondPage);

		this.fFirstPage.init(getSelection(), getActivePart());
	}

	private static boolean hasSourcePath(IJavaProject javaProject, IPath path) {
		if (path != null) {
			IPath pathInProject = javaProject.getProject().getFullPath().append(path);
			try {
				for (IClasspathEntry entry : javaProject.getRawClasspath()) {
					if (entry.getEntryKind() == IClasspathEntry.CPE_SOURCE
							&& pathInProject.equals(entry.getPath())) {
						return true;
					}
				}
			} catch (Throwable exception) {
				//
			}
		}
		return false;
	}

	private static String buildInvalidOutputPathMessageFragment(IJavaProject javaProject) {
		StringBuilder sourceFolders = new StringBuilder();
		try {
			for (IClasspathEntry entry : javaProject.getRawClasspath()) {
				if (entry.getEntryKind() == IClasspathEntry.CPE_SOURCE) {
					sourceFolders.append("\t"); //$NON-NLS-1$
					sourceFolders.append(entry.getPath().toOSString());
					sourceFolders.append("\n"); //$NON-NLS-1$
				}
			}
		} catch (Throwable exception) {
			//
		}
		return sourceFolders.toString();
	}

	private static String toOSString(IPath path) {
		if (path == null) {
			return Utilities.EMPTY_STRING;
		}
		return path.toOSString();
	}

	/** Validate the SARL properties of the new projects.
	 *
	 * @param element - the created element
	 * @return validity
	 */
	protected boolean validateSARLSpecificElements(IJavaElement element) {
		IJavaProject javaProject = (IJavaProject) element;
		// Check if the "SARL" generation directory is a source folder.
		IPath outputPath = SARLPreferences.getSARLOutputPathFor(javaProject.getProject());

		if (outputPath == null) {
			String message = MessageFormat.format(
					Messages.BuildSettingWizardPage_0,
					SARLConfig.FOLDER_SOURCE_GENERATED);
			IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
			handleFinishException(getShell(), new InvocationTargetException(new CoreException(status)));
			return false;
		}
		if (!hasSourcePath(javaProject, outputPath)) {
			String message = MessageFormat.format(
					Messages.SARLProjectCreationWizard_0,
					toOSString(outputPath),
					buildInvalidOutputPathMessageFragment(javaProject));
			IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
			handleFinishException(getShell(), new InvocationTargetException(new CoreException(status)));
			return false;
		}
		return true;
	}

	@Override
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
		// use the full progress monitor
		this.fSecondPage.performFinish(monitor);
	}

	@Override
	public boolean performFinish() {
		boolean res = super.performFinish();
		if (res) {
			final IJavaElement newElement;
			try {
				newElement = getCreatedElement();
			} catch (Throwable e) {
				handleFinishException(getShell(), new InvocationTargetException(e));
				return false;
			}

			// Validate the SARL specific elements
			if (!validateSARLSpecificElements(newElement)) {
				return false;
			}

			IWorkingSet[] workingSets = this.fFirstPage.getWorkingSets();
			if (workingSets.length > 0) {
				PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(newElement, workingSets);
			}

			BasicNewProjectResourceWizard.updatePerspective(this.fConfigElement);
			selectAndReveal(this.fSecondPage.getJavaProject().getProject());

			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					IWorkbenchPart activePart = getActivePart();
					if (activePart instanceof IPackagesViewPart) {
						PackageExplorerPart view = PackageExplorerPart.openInActivePerspective();
						view.tryToReveal(newElement);
					}
				}
			});
		}
		return res;
	}

	/** Replies the active part in the workbench.
	 *
	 * @return the active part.
	 */
	IWorkbenchPart getActivePart() {
		IWorkbenchWindow activeWindow = getWorkbench().getActiveWorkbenchWindow();
		if (activeWindow != null) {
			IWorkbenchPage activePage = activeWindow.getActivePage();
			if (activePage != null) {
				return activePage.getActivePart();
			}
		}
		return null;
	}

	@Override
	protected void handleFinishException(Shell shell, InvocationTargetException exception) {
		String title = NewWizardMessages.JavaProjectWizard_op_error_title;
		String message = NewWizardMessages.JavaProjectWizard_op_error_create_message;
		ExceptionHandler.handle(exception, getShell(), title, message);
	}

	@Override
	public void setInitializationData(IConfigurationElement cfig, String propertyName, Object data) {
		this.fConfigElement = cfig;
	}

	@Override
	public boolean performCancel() {
		this.fSecondPage.performCancel();
		return super.performCancel();
	}

	@Override
	public IJavaElement getCreatedElement() {
		IJavaProject javaProject = this.fSecondPage.getJavaProject();

		try {
			addNatures(javaProject.getProject());

			// Set the SRE configuration
			IProject project = javaProject.getProject();
			ISREInstall sre = this.fFirstPage.getSRE();
			boolean useDefaultSRE = (sre == null || this.fFirstPage.isSystemDefaultSRE());
			QualifiedName qn = RuntimeEnvironmentPropertyPage.qualify(
					RuntimeEnvironmentPropertyPage.PROPERTY_NAME_HAS_PROJECT_SPECIFIC);
			project.setPersistentProperty(qn, Boolean.toString(!useDefaultSRE));
			if (!useDefaultSRE) {
				assert (sre != null);
				qn = RuntimeEnvironmentPropertyPage.qualify(
						RuntimeEnvironmentPropertyPage.PROPERTY_NAME_USE_SYSTEM_WIDE_SRE);
				project.setPersistentProperty(qn, Boolean.FALSE.toString());
				qn = RuntimeEnvironmentPropertyPage.qualify(
						RuntimeEnvironmentPropertyPage.PROPERTY_NAME_SRE_INSTALL_ID);
				project.setPersistentProperty(qn, sre.getId());
			}
		} catch (Throwable e) {
			throw new RuntimeException(e);
		}

		return javaProject;
	}

	private static void addNatures(IProject project) throws CoreException {
		final IProjectDescription description = project.getDescription();
		final List<String> natures = new ArrayList<>(Arrays.asList(description.getNatureIds()));
		natures.add(0, SARLEclipseConfig.NATURE_ID);
		natures.add(1, SARLEclipseConfig.XTEXT_NATURE_ID);
		// natures.add(2, JavaCore.NATURE_ID); not necessary since the project is already a java project

		final String[] newNatures = natures.toArray(new String[natures.size()]);
		final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);

		// check the status and decide what to do
		if (status.getCode() == IStatus.OK) {
			description.setNatureIds(newNatures);
			IProgressMonitor monitor = new NullProgressMonitor();
			project.setDescription(description, monitor);
		} else {
			JavaPlugin.logErrorStatus(
					Messages.SARLProjectNewWizard_4,
					status);
		}
	}
}
