/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2021 the original authors or authors.
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

/** Messages for the wizard "new SARL project".
 *
 * @author $Author: ngaud$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public class Messages extends NLS {

	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$

	public static String BuildSettingWizardPage_0;

	public static String JavaProjectWizard_op_error_create_message;

	public static String JavaProjectWizard_op_error_title;

	public static String MainProjectPage_0;public static String NewJavaProjectWizardPageTwo_DeleteCorruptProjectFile_message;

	public static String NewJavaProjectWizardPageTwo_error_message;

	public static String NewJavaProjectWizardPageTwo_error_remove_message;

	public static String NewJavaProjectWizardPageTwo_error_remove_title;

	public static String NewJavaProjectWizardPageTwo_error_title;

	public static String NewJavaProjectWizardPageTwo_monitor_init_build_path;

	public static String NewJavaProjectWizardPageTwo_operation_create;

	public static String NewJavaProjectWizardPageTwo_operation_initialize;

	public static String NewJavaProjectWizardPageTwo_operation_remove;

	public static String NewJavaProjectWizardPageTwo_problem_backup;

	public static String NewJavaProjectWizardPageTwo_problem_restore_classpath;

	public static String NewJavaProjectWizardPageTwo_problem_restore_project;

	public static String SARLProjectCreationWizard_0;

	public static String SARLProjectNewWizard_0;
	public static String SARLProjectNewWizard_1;
	public static String SARLProjectNewWizard_2;
	public static String SARLProjectNewWizard_3;

	public static String SARLProjectNewWizard_4;

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}

}
