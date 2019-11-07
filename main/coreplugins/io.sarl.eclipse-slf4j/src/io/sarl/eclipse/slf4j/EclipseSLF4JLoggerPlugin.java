/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

package io.sarl.eclipse.slf4j;

import org.eclipse.core.runtime.Plugin;
import org.osgi.framework.BundleContext;
import org.slf4j.LoggerFactory;

/**
 * Utility functions for the plugin.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
public class EclipseSLF4JLoggerPlugin extends Plugin {

	/** Identifier of the plugin.
	 */
	public static final String PLUGIN_ID = "io.sarl.eclipse-slf4j"; //$NON-NLS-1$

	private static EclipseSLF4JLoggerPlugin plugin;

	/** Construct an Eclipse-SLF4J plugin for SARL.
	 */
	public EclipseSLF4JLoggerPlugin() {
		//
	}

	@Override
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		// In order to be used by the SLF4J API, the context class loader must be
		// overridden with the OSGI class loader.
		final ClassLoader osgiClassLoader = getClass().getClassLoader();
		Thread.currentThread().setContextClassLoader(osgiClassLoader);
		// Force the loading of the SLF4J logger from the class path of this plugin
		LoggerFactory.getILoggerFactory();
	}

	@Override
	public void stop(BundleContext context) throws Exception {
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance.
	 *
	 * @return the shared instance.
	 */
	public static EclipseSLF4JLoggerPlugin getDefault() {
		return plugin;
	}

}
