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

package io.sarl.lang;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;

/**
 * Bundle activator for the SARL language module.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLLangActivator implements BundleActivator {

	private static SARLLangActivator singleton;

	/** Constructor the the activator.
	 */
	public SARLLangActivator() {
		//
	}

	/** Replies the activator of the bundle.
	 *
	 * @return the activator of the bundle.
	 */
	public static SARLLangActivator getActivator() {
		return singleton;
	}

	@Override
	public void start(BundleContext context) throws Exception {
		singleton = this;
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		//
	}

}


