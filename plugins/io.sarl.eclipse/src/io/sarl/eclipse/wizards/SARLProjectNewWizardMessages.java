/**
 * 
 */
package io.sarl.eclipse.wizards;

import org.eclipse.osgi.util.NLS;

/**
 * @author ngaud
 *
 */
public class SARLProjectNewWizardMessages extends NLS {
	private static final String BUNDLE_NAME = "io.sarl.eclipse.wizards.SARLProjectNewWizard"; //$NON-NLS-1$
	
	/**
	 */
	public static String SARLProjectNewWizard_WIZARD_NAME;
	/**
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_1_DESCRIPTION;
	/**
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_2_DESCRIPTION;
	/**
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_NAME;
	
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, SARLProjectNewWizardMessages.class);
	}

	private SARLProjectNewWizardMessages() {
	}
}
