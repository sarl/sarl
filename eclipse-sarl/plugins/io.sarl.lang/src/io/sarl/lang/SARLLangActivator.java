/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2016 the original authors or authors.
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
 * Bundle activator for the SARL language module.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SARLLangActivator implements BundleActivator {

	private static SARLLangActivator singleton;

	private String minimalJdkVersion;

	private String minimalXtextVersion;

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
		final ResourceBundle bundle = ResourceBundle.getBundle("OSGI-INF/l10n/bundle"); //$NON-NLS-1$
		this.minimalJdkVersion = bundle.getString("min.jdk.version"); //$NON-NLS-1$
		this.minimalXtextVersion = bundle.getString("min.xtext.version"); //$NON-NLS-1$
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		//
	}

	/** Returns the minimal JDK version required by SARL.
	 *
	 * <p>The version is read from the bundle's properties.
	 *
	 * @return the minimal JDK version required by SARL.
	 */
	public String getMinimalJdkVersion() {
		return this.minimalJdkVersion;
	}

	/** Returns the minimal Xbase version required by SARL.
	 *
	 * <p>The version is read from the bundle's properties.
	 *
	 * @return the minimal Xbase version required by SARL.
	 */
	public String getMinimalXtextVersion() {
		return this.minimalXtextVersion;
	}

}


