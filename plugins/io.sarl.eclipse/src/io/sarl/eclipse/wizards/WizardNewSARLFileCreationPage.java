/**
 * 
 */
package io.sarl.eclipse.wizards;

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

	private static final String WIZARD_NAME = "SARL File Wizard";
	private static final String PAGE_TITLE = "SARL File Wizard";
	private static final String PAGE_DESCRIPTION = "Create a SARL File";
	
	
	public static final String SARL_FILE_EXTENSION = "sarl";
	
	/**
	 * 
	 * @param selection
	 */
    public WizardNewSARLFileCreationPage(IStructuredSelection selection) {
        super(WIZARD_NAME, selection);
 
        setTitle(PAGE_TITLE);
        setDescription(PAGE_DESCRIPTION);
        setFileExtension(SARL_FILE_EXTENSION);
    }
 
   /* @Override
    protected InputStream getInitialContents() {
        String sarlTemplate = 
        //Addd the package by default
        return new ByteArrayInputStream(sarlTemplate.getBytes());
    }*/
	
	
}
