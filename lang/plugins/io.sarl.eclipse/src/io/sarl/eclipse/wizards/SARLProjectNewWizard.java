/**
 * 
 */
package io.sarl.eclipse.wizards;

import io.sarl.eclipse.projects.SARLProjectSupport;

import java.net.URI;

import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExecutableExtension;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.dialogs.WizardNewProjectCreationPage;
import org.eclipse.ui.wizards.newresource.BasicNewProjectResourceWizard;



/**
 * SARL project NEW wizard
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLProjectNewWizard extends Wizard implements INewWizard, IExecutableExtension {

	private static final String WIZARD_NAME = SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_NAME;
	private static final String PAGE_NAME = SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_PAGE_NAME;
	private static final String PAGE_DESCRIPTION = SARLProjectNewWizardMessages.SARLProjectNewWizard_WIZARD_PAGE_DESCRIPTION;

	private WizardNewProjectCreationPage pageOne;
	
	private IConfigurationElement configurationElement;

	/**
	 * Create a new Project NEW wizard for SARL project
	 */
	public SARLProjectNewWizard() {
		setWindowTitle(WIZARD_NAME);
	}

	@Override
	public void addPages() {
		super.addPages();

		this.pageOne = new WizardNewProjectCreationPage(PAGE_NAME);
		this.pageOne.setTitle(PAGE_NAME);
		this.pageOne.setDescription(PAGE_DESCRIPTION);

		addPage(this.pageOne);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
	 */
	@Override
	public void init(IWorkbench arg0, IStructuredSelection arg1) {
		// TODO Auto-generated method stub

	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.jface.wizard.Wizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		String name = this.pageOne.getProjectName();
		URI location = null;
		if (!this.pageOne.useDefaults()) {
			location = this.pageOne.getLocationURI();
		} // else location == null

		SARLProjectSupport.createProject(name, location, this.pageOne.useDefaults());

		BasicNewProjectResourceWizard.updatePerspective(this.configurationElement);
		
		return true;
	}

	/* (non-Javadoc)
	 * @see org.eclipse.core.runtime.IExecutableExtension#setInitializationData(org.eclipse.core.runtime.IConfigurationElement, java.lang.String, java.lang.Object)
	 */
	@Override
	public void setInitializationData(IConfigurationElement config, String propertyName, Object data) throws CoreException {
		this.configurationElement = config;
		
	}

}
