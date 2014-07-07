/**
 * 
 */
package io.sarl.eclipse.wizards.newproject;

import org.eclipse.osgi.util.NLS;

/**
 * @author ngaud
 *
 */
class SARLProjectNewWizardMessages extends NLS {
	private static final String BUNDLE_NAME = SARLProjectNewWizard.class.getName();
	
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
