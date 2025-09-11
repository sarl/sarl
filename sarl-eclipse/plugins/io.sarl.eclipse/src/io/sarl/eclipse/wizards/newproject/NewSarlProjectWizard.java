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

package io.sarl.eclipse.wizards.newproject;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.lang.reflect.InvocationTargetException;
import java.net.URL;
import java.nio.charset.Charset;
import java.text.MessageFormat;
import java.util.regex.Pattern;

import com.google.common.base.Strings;
import com.google.common.io.Resources;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IClasspathEntry;
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
import org.eclipse.ui.IWorkbenchPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;
import org.eclipse.xtext.util.RuntimeIOException;
import org.eclipse.xtext.util.StringInputStream;

import io.sarl.apputils.uiextensions.Utilities;
import io.sarl.eclipse.SARLEclipseConfig;
import io.sarl.eclipse.SARLEclipsePlugin;
import io.sarl.eclipse.natures.SARLProjectConfigurator;
import io.sarl.eclipse.properties.RuntimeEnvironmentPropertyPage;
import io.sarl.lang.SARLConfig;
import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.ui.preferences.SARLPreferences;

/**
 * SARL new project wizard.
 * Most part of the code of this class comes from {@link JavaProjectWizard}.
 *
 * @author <a href="http://www.ciad-lab.fr/nicolas_gaud">Nicolas Gaud</a>
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.15.1 20250911-224827
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 */
@SuppressWarnings("restriction")
public class NewSarlProjectWizard extends NewElementWizard implements IExecutableExtension {

	/** The base name of the file that contains a template of a pom file.
	 *
	 * @since 0.8
	 * @see #getPomTemplateLocation()
	 */
	public static final String POM_TEMPLATE_BASENAME = "pom_template.xml"; //$NON-NLS-1$

	private static final String DEFAULT_MAVEN_PROJECT_VERSION = "0.0.1-SNAPSHOT"; //$NON-NLS-1$

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
	 * @param pageOne reference to the first page of the wizard.
	 * @param pageTwo reference to the second page of the wizard.
	 */
	public NewSarlProjectWizard(MainProjectWizardPage pageOne, BuildSettingWizardPage pageTwo) {
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		this.firstPage = pageOne;
		this.secondPage = pageTwo;
	}

	/** Replies the location of a template for the pom files.
	 *
	 * @return the location. Should be never {@code null}.
	 * @since 0.8
	 * @see #POM_TEMPLATE_BASENAME
	 */
	public static URL getPomTemplateLocation() {
		final var url = Resources.getResource(NewSarlProjectWizard.class, POM_TEMPLATE_BASENAME);
		assert url != null;
		return url;
	}

	@Override
	public void init(IWorkbench workbench, IStructuredSelection currentSelection) {
		super.init(workbench, currentSelection);
		setDefaultPageImageDescriptor(SARLEclipsePlugin.getDefault().getImageDescriptor(
				SARLEclipseConfig.NEW_PROJECT_WIZARD_DIALOG_IMAGE));
		setWindowTitle(Messages.NewSarlProjectWizard_0);
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
			final var pathInProject = javaProject.getProject().getFullPath().append(path);
			try {
				for (final var entry : javaProject.getRawClasspath()) {
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
		final var sourceFolders = new StringBuilder();
		try {
			for (final var entry : javaProject.getRawClasspath()) {
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
	 * @param javaProject the created element
	 * @return validity
	 */
	protected boolean validateSARLSpecificElements(IJavaProject javaProject) {
		// Check if the "SARL" generation directory is a source folder.
		final var outputPath = SARLPreferences.getSARLOutputPathFor(javaProject.getProject());

		if (outputPath == null) {
			final var message = MessageFormat.format(
					Messages.NewSarlProjectWizard_1,
					SARLConfig.FOLDER_SOURCE_GENERATED);
			final var status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
			handleFinishException(getShell(), new InvocationTargetException(new CoreException(status)));
			return false;
		}
		if (!hasSourcePath(javaProject, outputPath)) {
			final var message = MessageFormat.format(
					Messages.NewSarlProjectWizard_2,
					toOSString(outputPath),
					buildInvalidOutputPathMessageFragment(javaProject));
			final var status = SARLEclipsePlugin.getDefault().createStatus(IStatus.ERROR, message);
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
		final var res = super.performFinish();
		if (res) {
			final IJavaProject newElement;
			try {
				newElement = getCreatedElement();
			} catch (Throwable e) {
				handleFinishException(getShell(), new InvocationTargetException(e));
				return false;
			}

			// Create the default Maven pom.xml
			createDefaultMavenPom(newElement, this.firstPage.getCompilerCompliance());

			// Force SARL configuration
			SARLProjectConfigurator.configureSARLProject(newElement.getProject(),
					true, false, false, new NullProgressMonitor());

			// Validate the SARL specific elements
			if (!validateSARLSpecificElements(newElement)) {
				return false;
			}

			final var workingSets = this.firstPage.getWorkingSets();
			if (workingSets.length > 0) {
				PlatformUI.getWorkbench().getWorkingSetManager().addToWorkingSets(newElement, workingSets);
			}

			try {
				newElement.getProject().refreshLocal(IResource.DEPTH_INFINITE, new NullProgressMonitor());
			} catch (CoreException e) {
				handleFinishException(getShell(), new InvocationTargetException(e));
				return false;
			}
			BasicNewProjectResourceWizard.updatePerspective(this.fConfigElement);
			selectAndReveal(newElement.getProject());

			Display.getDefault().asyncExec(new Runnable() {
				@Override
				public void run() {
					final var activePart = getActivePart();
					if (activePart instanceof IPackagesViewPart) {
						final var view = PackageExplorerPart.openInActivePerspective();
						view.tryToReveal(newElement);
					}
				}
			});
		}
		return res;
	}

	/** Create the default Maven pom file for the project.
	 *
	 * <p>Even if the project has not the Maven nature when it is created with this wizard,
	 * the pom file is created in order to let the developer to switch to the Maven nature easily.
	 *
	 * @param project the new project.
	 * @param compilerCompliance the Java version that is supported by the project.
	 */
	protected void createDefaultMavenPom(IJavaProject project, String compilerCompliance) {
		final var pomFile = project.getProject().getFile("pom.xml"); //$NON-NLS-1$
		// Do not create the pom if already present.
		if (!pomFile.exists()) {
			// Get the template resource.
			final var templateUrl = getPomTemplateLocation();
			if (templateUrl != null) {
				final var compliance = Strings.isNullOrEmpty(compilerCompliance)
						? SARLVersion.MINIMAL_JDK_VERSION_IN_SARL_PROJECT_CLASSPATH : compilerCompliance;
				final var groupId = getDefaultMavenGroupId();
				// Read the template and do string replacement.
				final var content = new StringBuilder();
				try (var reader = new BufferedReader(new InputStreamReader(templateUrl.openStream()))) {
					var line = reader.readLine();
					while (line != null) {
						line = line.replaceAll(Pattern.quote("@GROUP_ID@"), groupId); //$NON-NLS-1$
						line = line.replaceAll(Pattern.quote("@PROJECT_NAME@"), project.getElementName()); //$NON-NLS-1$
						line = line.replaceAll(Pattern.quote("@PROJECT_VERSION@"), DEFAULT_MAVEN_PROJECT_VERSION); //$NON-NLS-1$
						line = line.replaceAll(Pattern.quote("@SARL_VERSION@"), SARLVersion.SARL_RELEASE_VERSION_MAVEN); //$NON-NLS-1$
						line = line.replaceAll(Pattern.quote("@JAVA_VERSION@"), compliance); //$NON-NLS-1$
						line = line.replaceAll(Pattern.quote("@FILE_ENCODING@"), Charset.defaultCharset().displayName()); //$NON-NLS-1$
						content.append(line).append("\n"); //$NON-NLS-1$
						line = reader.readLine();
					}
				} catch (IOException exception) {
					throw new RuntimeIOException(exception);
				}
				// Write the pom
				try (var is = new StringInputStream(content.toString())) {
					pomFile.create(is, true, new NullProgressMonitor());
				} catch (CoreException exception) {
					throw new RuntimeException(exception);
				} catch (IOException exception) {
					throw new RuntimeIOException(exception);
				}
			}
		}
	}

	/** Replies the default group id for a maven project.
	 *
	 * @return the default group id, never {@code null} nor empty string.
	 */
	@SuppressWarnings("static-method")
	protected String getDefaultMavenGroupId() {
		final var userdomain = System.getenv("userdomain"); //$NON-NLS-1$
		if (Strings.isNullOrEmpty(userdomain)) {
			return "com.foo"; //$NON-NLS-1$
		}
		final var elements = userdomain.split(Pattern.quote(".")); //$NON-NLS-1$
		final var groupId = new StringBuilder();
		for (var i = elements.length - 1; i >= 0; --i) {
			if (groupId.length() > 0) {
				groupId.append("."); //$NON-NLS-1$
			}
			groupId.append(elements[i]);
		}
		return groupId.toString();
	}

	/** Replies the active part in the workbench.
	 *
	 * @return the active part.
	 */
	IWorkbenchPart getActivePart() {
		final var activeWindow = getWorkbench().getActiveWorkbenchWindow();
		if (activeWindow != null) {
			final var activePage = activeWindow.getActivePage();
			if (activePage != null) {
				return activePage.getActivePart();
			}
		}
		return null;
	}

	@Override
	protected void handleFinishException(Shell shell, InvocationTargetException exception) {
		final var title = NewWizardMessages.JavaProjectWizard_op_error_title;
		final var message = NewWizardMessages.JavaProjectWizard_op_error_create_message;
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
	public IJavaProject getCreatedElement() {
		final var javaProject = this.secondPage.getJavaProject();

		try {
			// Set the SRE configuration
			final var project = javaProject.getProject();
			final var sre = this.firstPage.getSRE();
			final var useDefaultSRE = sre == null || this.firstPage.isSystemDefaultSRE();
			var qn = RuntimeEnvironmentPropertyPage.qualify(
					RuntimeEnvironmentPropertyPage.PROPERTY_NAME_HAS_PROJECT_SPECIFIC);
			project.setPersistentProperty(qn, Boolean.toString(!useDefaultSRE));
			if (!useDefaultSRE && sre != null) {
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
