/**
 * 
 */
package io.sarl.eclipse.wizards;

import io.sarl.eclipse.images.EclipseSARLImages;

import org.eclipse.jface.viewers.IStructuredSelection;
import org.eclipse.ui.dialogs.WizardNewFileCreationPage;

/**
 * First page of the SARL new file wizard.
 * 
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class WizardNewSARLFileCreationPage extends WizardNewFileCreationPage {

	private static final String WIZARD_NAME = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_TITLE = "SARL File Wizard"; //$NON-NLS-1$
	private static final String PAGE_DESCRIPTION = "Create a SARL File"; //$NON-NLS-1$
	private static final String SARL_FILE_EXTENSION = "sarl"; //$NON-NLS-1$
	
	/**
	 * 
	 * @param selection
	 */
    public WizardNewSARLFileCreationPage(IStructuredSelection selection) {
        super(WIZARD_NAME, selection);
 
        setTitle(PAGE_TITLE);
        setDescription(PAGE_DESCRIPTION);
        setFileExtension(SARL_FILE_EXTENSION);
        setImageDescriptor(EclipseSARLImages.getImageDescriptor(
        		EclipseSARLImages.NEW_FILE_WIZARD_DIALOG_IMAGE));
    }
 
   /* @Override
    protected InputStream getInitialContents() {
        String sarlTemplate = 
        //Addd the package by default
        return new ByteArrayInputStream(sarlTemplate.getBytes());
    }*/
	
	
}
