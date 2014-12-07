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
package io.sarl.lang;

import java.util.ResourceBundle;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;



/**
 * Set of SARL keywords that are added to the xtext/xbase ones.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLConstants implements BundleActivator {

	/** The minimal JDK version required by SARL.
	 */
	public static String MINIMAL_JDK_VERSION;

	/** The minimal Xbase version required by SARL.
	 */
	public static String MINIMAL_XBASE_VERSION;

	/**
	 */
	public SARLConstants() {
		//
	}

	@Override
	public void start(BundleContext context) throws Exception {
		ResourceBundle bundle = ResourceBundle.getBundle("OSGI-INF/l10n/bundle"); //$NON-NLS-1$
		MINIMAL_JDK_VERSION = bundle.getString("min.jdk.version"); //$NON-NLS-1$
		MINIMAL_XBASE_VERSION = bundle.getString("min.xtext.version"); //$NON-NLS-1$
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		//
	}

}


