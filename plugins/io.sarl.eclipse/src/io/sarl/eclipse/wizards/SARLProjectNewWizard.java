/**
 * 
 */
package io.sarl.eclipse.wizards;

import java.lang.reflect.InvocationTargetException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.eclipse.core.resources.IProject;
import org.eclipse.core.resources.IProjectDescription;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jdt.core.IJavaElement;
import org.eclipse.jdt.core.IJavaProject;
import org.eclipse.jdt.core.JavaModelException;
import org.eclipse.jdt.internal.ui.JavaPlugin;
import org.eclipse.jdt.internal.ui.JavaPluginImages;
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

/**
 * SARL new project wizard.
 * Most part of the code of this class comes from {@link JavaProjectWizard}
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectNewWizard extends NewElementWizard implements IExecutableExtension {

	private NewSARLProjectWizardPageOne fFirstPage;
	private NewSARLProjectWizardPageTwo fSecondPage;

	private IConfigurationElement fConfigElement;

	/**
	 * The name of this SARL new project wizard
	 */
	private static final String WIZARD_NAME = SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_NAME;

	/**
	 * ID of the project nature defined by XText
	 */
	private static final String XTEXT_NATURE_ID = "org.eclipse.xtext.ui.shared.xtextNature"; //$NON-NLS-1$

	/**
	 * 
	 */
	public SARLProjectNewWizard() {
		this(null, null);
	}

	
	/**
	 * 
	 * @param pageOne
	 * @param pageTwo
	 */
	public SARLProjectNewWizard(NewSARLProjectWizardPageOne pageOne, NewSARLProjectWizardPageTwo pageTwo) {
		setDefaultPageImageDescriptor(JavaPluginImages.DESC_WIZBAN_NEWJPRJ);
		setDialogSettings(JavaPlugin.getDefault().getDialogSettings());
		setWindowTitle(WIZARD_NAME);

		this.fFirstPage = pageOne;
		this.fSecondPage = pageTwo;
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.Wizard#addPages()
	 */
	@Override
	public void addPages() {
		if (this.fFirstPage == null)
			this.fFirstPage = new NewSARLProjectWizardPageOne();
		addPage(this.fFirstPage);

		if (this.fSecondPage == null)
			this.fSecondPage = new NewSARLProjectWizardPageTwo(this.fFirstPage);
		addPage(this.fSecondPage);

		this.fFirstPage.init(getSelection(), getActivePart());
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jdt.internal.ui.wizards.NewElementWizard#finishPage(org.eclipse.core.runtime.IProgressMonitor)
	 */
	@Override
	protected void finishPage(IProgressMonitor monitor) throws InterruptedException, CoreException {
		this.fSecondPage.performFinish(monitor); // use the full progress monitor
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.IWizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		boolean res = super.performFinish();
		if (res) {
			final IJavaElement newElement = getCreatedElement();

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
	protected void handleFinishException(Shell shell, InvocationTargetException e) {
		String title = NewWizardMessages.JavaProjectWizard_op_error_title;
		String message = NewWizardMessages.JavaProjectWizard_op_error_create_message;
		ExceptionHandler.handle(e, getShell(), title, message);
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
		} catch (JavaModelException e) {
			e.printStackTrace();
		} catch (CoreException e) {
			e.printStackTrace();
		}

		return javaProject;
	}

	private static void addNatures(IProject project) throws CoreException {
		final IProjectDescription description = project.getDescription();
		final List<String> natures = new ArrayList<>(Arrays.asList(description.getNatureIds()));
		natures.add(0, io.sarl.eclipse.natures.SARLProjectNature.NATURE_ID);
		natures.add(1, XTEXT_NATURE_ID);
		// natures.add(2, JavaCore.NATURE_ID); not necessary since the project is already a java project

		final String[] newNatures = natures.toArray(new String[natures.size()]);
		final IStatus status = ResourcesPlugin.getWorkspace().validateNatureSet(newNatures);

		// check the status and decide what to do
		if (status.getCode() == IStatus.OK) {
			description.setNatureIds(newNatures);
			IProgressMonitor monitor = new NullProgressMonitor();
			project.setDescription(description, monitor);
		} else {
			// TODO raise a user error
		}
	}
}
