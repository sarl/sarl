/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2024 SARL.io, the Original Authors and Main Authors
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

package io.sarl.eclipse.runtime;

import org.eclipse.osgi.util.NLS;

/** Messages for the sarl runtime environment.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version io.sarl.eclipse 0.14.0 20241106-161410
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public class Messages extends NLS {
	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$
	public static String SARLRuntime_0;
	public static String SARLRuntime_1;
	public static String SARLRuntime_2;
	public static String SARLRuntime_3;
	public static String AbstractSREInstall_0;
	public static String AbstractSREInstall_1;
	public static String AbstractSREInstall_2;
	public static String AbstractSREInstall_3;
	public static String AbstractSREInstall_4;
	public static String StandardSREInstall_0;
	public static String StandardSREInstall_1;
	public static String StandardSREInstall_2;
	public static String StandardSREInstall_3;
	public static String StandardSREInstall_4;
	public static String StandardSREInstall_5;
	public static String SREConfigurationBlock_0;
	public static String SREConfigurationBlock_1;
	public static String SREConfigurationBlock_2;
	public static String SREConfigurationBlock_3;
	public static String SREConfigurationBlock_4;
	public static String SREConfigurationBlock_5;
	public static String SREConfigurationBlock_6;
	public static String SREConfigurationBlock_7;
	public static String SREConfigurationBlock_8;
	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	private Messages() {
	}
}
