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

package io.sarl.eclipse.wizards.newproject;

import java.lang.reflect.InvocationTargetException;
import java.text.MessageFormat;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
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
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IWorkbench;
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

	private MainProjectWizardPage firstPage;

	private BuildSettingWizardPage secondPage;

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
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		this.firstPage = pageOne;
		this.secondPage = pageTwo;
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		setDefaultPageImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
		setWindowTitle(Messages.SARLProjectNewWizard_0);
	}

	@Override
	public void addPages() {
		if (this.firstPage == null) {
			this.firstPage = new MainProjectWizardPage();
		}
		addPage(this.firstPage);

		if (this.secondPage == null) {
			this.secondPage = new BuildSettingWizardPage(this.firstPage);
		}
		addPage(this.secondPage);

		this.firstPage.init(getSelection(), getActivePart());
	}

	private static boolean hasSourcePath(IJavaProject javaProject, IPath path) {
		if (path != null) {
			final IPath pathInProject = javaProject.getProject().getFullPath().append(path);
			try {
				for (final IClasspathEntry entry : javaProject.getRawClasspath()) {
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
		final StringBuilder sourceFolders = new StringBuilder();
		try {
			for (final IClasspathEntry entry : javaProject.getRawClasspath()) {
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
		final IJavaProject javaProject = (IJavaProject) element;
		// Check if the "SARL" generation directory is a source folder.
		final IPath outputPath = SARLPreferences.getSARLOutputPathFor(javaProject.getProject());

		if (outputPath == null) {
			final String message = MessageFormat.format(
					Messages.BuildSettingWizardPage_0,
					SARLConfig.FOLDER_SOURCE_GENERATED);
			final IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
			handleFinishException(getShell(), new InvocationTargetException(new CoreException(status)));
			return false;
		}
		if (!hasSourcePath(javaProject, outputPath)) {
			final String message = MessageFormat.format(
					Messages.SARLProjectCreationWizard_0,
					toOSString(outputPath),
					buildInvalidOutputPathMessageFragment(javaProject));
			final IStatus status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
			handleFinishException(getShell(), new InvocationTargetException(new CoreException(status)));
			return false;
		}
		return true;
	}

	@Override
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
		// use the full progress monitor
		this.secondPage.performFinish(monitor);
	}

	@Override
	public boolean performFinish() {
		final boolean res = super.performFinish();
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

			final IWorkingSet[] workingSets = this.firstPage.getWorkingSets();
			if (workingSets.length > 0) {
				PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(newElement, workingSets);
			}

			BasicNewProjectResourceWizard.updatePerspective(this.fConfigElement);
			selectAndReveal(this.secondPage.getJavaProject().getProject());

			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					final IWorkbenchPart activePart = getActivePart();
					if (activePart instanceof IPackagesViewPart) {
						final PackageExplorerPart view = PackageExplorerPart.openInActivePerspective();
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
		final IWorkbenchWindow activeWindow = getWorkbench().getActiveWorkbenchWindow();
		if (activeWindow != null) {
			final IWorkbenchPage activePage = activeWindow.getActivePage();
			if (activePage != null) {
				return activePage.getActivePart();
			}
		}
		return null;
	}

	@Override
	protected void handleFinishException(Shell shell, InvocationTargetException exception) {
		final String title = NewWizardMessages.JavaProjectWizard_op_error_title;
		final String message = NewWizardMessages.JavaProjectWizard_op_error_create_message;
		ExceptionHandler.handle(exception, getShell(), title, message);
	}

	@Override
	public void setInitializationData(IConfigurationElement cfig, String propertyName, Object data) {
		this.fConfigElement = cfig;
	}

	@Override
	public boolean performCancel() {
		this.secondPage.performCancel();
		return super.performCancel();
	}

	@Override
	public IJavaElement getCreatedElement() {
		final IJavaProject javaProject = this.secondPage.getJavaProject();

		try {
			// Set the SRE configuration
			final IProject project = javaProject.getProject();
			final ISREInstall sre = this.firstPage.getSRE();
			final boolean useDefaultSRE = sre == null || this.firstPage.isSystemDefaultSRE();
			QualifiedName qn = RuntimeEnvironmentPropertyPage.qualify(
					RuntimeEnvironmentPropertyPage.PROPERTY_NAME_HAS_PROJECT_SPECIFIC);
			project.setPersistentProperty(qn, Boolean.toString(!useDefaultSRE));
			if (!useDefaultSRE) {
				assert sre != null;
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

}
