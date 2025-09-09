/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2025 SARL.io, the Original Authors and Main Authors.
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
package io.sarl.apputils.bootiqueapp.config;

import org.eclipse.osgi.util.NLS;

/** Messages for the SRE configuration.
 * 
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version bootiqueapp 0.15.0 20250909-115749
 * @mavengroupid io.sarl.apputils
 * @mavenartifactid bootiqueapp
 * @ExcludeFromApidoc
 */
@SuppressWarnings("all")
public final class Messages extends NLS {

	private static final String BUNDLE_NAME = Messages.class.getPackage().getName() + ".messages"; //$NON-NLS-1$

	static {
		// initialize resource bundle
		NLS.initializeMessages(BUNDLE_NAME, Messages.class);
	}

	public static String LogConfigModule_0;
	public static String LogConfigModule_1;
	public static String LogConfigModuleProvider_0;

	private Messages() {
		//
	}

}
