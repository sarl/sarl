/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014 Sebastian RODRIGUEZ, Nicolas GAUD, Stéphane GALLAND.
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

import org.eclipse.osgi.util.NLS;

/** Messages for the wozard "new SARL project".
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
final class SARLProjectNewWizardMessages extends NLS {

	private static final String BUNDLE_NAME = SARLProjectNewWizard.class.getName();

	//CHECKSTYLE:OFF

	/** Name of the wizard.
	 */
	public static String SARLProjectNewWizard_WIZARD_NAME;

	/** Description for the first page.
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_1_DESCRIPTION;

	/** Description for the second page.
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_2_DESCRIPTION;

	/** Name of the pages.
	 */
	public static String SARLProjectNewWizard_WIZARD_PAGE_NAME;

	//CHECKSTYLE:ON

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, SARLProjectNewWizardMessages.class);
	}

	private SARLProjectNewWizardMessages() {
	}

}
