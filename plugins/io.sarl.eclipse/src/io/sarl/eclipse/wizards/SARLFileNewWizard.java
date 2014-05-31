/**
 * 
 */
package io.sarl.eclipse.wizards;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.jface.wizard.Wizard;
import org.eclipse.ui.INewWizard;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.PartInitException;
import org.eclipse.ui.ide.IDE;

/**
 * SARL new file wizard.
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLFileNewWizard extends Wizard implements INewWizard {

	private IWorkbench workbench;
	private IStructuredSelection selection;
	private WizardNewSARLFileCreationPage pageOne;
	
	/* (non-Javadoc)
	 * @see org.eclipse.ui.IWorkbenchWizard#init(org.eclipse.ui.IWorkbench, org.eclipse.jface.viewers.IStructuredSelection)
	 */
	public void init(IWorkbench iworkbench, IStructuredSelection iselection) {
		this.workbench = iworkbench;
		this.selection = iselection;
		
	}
	
	@Override
	public void addPages() {
	    super.addPages();
	 
	    this.pageOne = new WizardNewSARLFileCreationPage(this.selection);
	 
	    addPage(this.pageOne);
	}

	/* (non-Javadoc)
	 * @see org.eclipse.jface.wizard.Wizard#performFinish()
	 */
	@Override
	public boolean performFinish() {
		boolean result = false;
		 
	    IFile file = this.pageOne.createNewFile();
	    result = file != null;
	 
	    if (result) {
	        try {
	            IDE.openEditor(this.workbench.getActiveWorkbenchWindow().getActivePage(), file);
	        } catch (PartInitException e) {
	            e.printStackTrace();
	        }
	    } // else no file created...result == false
	 
	    return result;
	}

}
