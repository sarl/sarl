/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.eclipse.log;

import org.eclipse.osgi.util.NLS;

/** Localized messages.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}

	public static String IssueInformationPage_0;
	public static String IssueInformationPage_1;
	public static String IssueInformationPage_2;
	public static String IssueInformationPage_3;
	public static String IssueInformationPage_4;
	public static String IssueInformationPage_5;
	public static String IssueInformationPage_6;
	public static String IssueInformationPage_7;
	public static String IssueInformationPage_8;
	public static String IssueInformationPage_9;
	public static String SubmitEclipseLogWizard_0;
	public static String SubmitEclipseLogWizard_1;
	public static String SubmitEclipseLogWizard_10;
	public static String SubmitEclipseLogWizard_11;
	public static String SubmitEclipseLogWizard_12;
	public static String SubmitEclipseLogWizard_13;
	public static String SubmitEclipseLogWizard_14;
	public static String SubmitEclipseLogWizard_15;
	public static String SubmitEclipseLogWizard_2;
	public static String SubmitEclipseLogWizard_3;
	public static String SubmitEclipseLogWizard_4;
	public static String SubmitEclipseLogWizard_5;
	public static String SubmitEclipseLogWizard_6;
	public static String SubmitEclipseLogWizard_8;
	public static String SubmitEclipseLogWizard_9;
}
