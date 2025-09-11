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

package io.sarl.lang.pythongenerator;

import org.osgi.framework.Bundle;
import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * Utility functions for the plugin.
 *
 * @author <a href="http://www.ciad-lab.fr/stephane_galland">St&eacute;phane Galland</a>
 * @version pythongenerator 0.15.1 20250911-224823
 * @mavengroupid io.sarl.lang
 * @mavenartifactid pythongenerator
 */
public class PyGeneratorPlugin implements BundleActivator {

	/**
	 * Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.lang.pythongenerator"; //$NON-NLS-1$

	/**
	 * Identifier of the preference container.
	 */
	public static final String PREFERENCE_ID = PLUGIN_ID;

	private static Bundle bundle;

	/** Replies the bundle of this plugin.
	 *
	 * @return the bundle.
	 */
	public static Bundle getBundle() {
		return bundle;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		bundle = context.getBundle();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		//
	}

}
