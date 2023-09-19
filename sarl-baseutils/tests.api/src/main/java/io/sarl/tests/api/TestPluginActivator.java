/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2023 SARL.io, the Original Authors and Main Authors
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
package io.sarl.tests.api;

import org.osgi.framework.BundleActivator;
import org.osgi.framework.BundleContext;


/** Activator of the bundle test.
 *
 * @author $Author: sgalland$
 * @version tests.api 0.13.0 20230919-093055
 * @mavengroupid io.sarl.baseutils
 * @mavenartifactid tests.api
 */
public class TestPluginActivator implements BundleActivator {

	/** Current bundle context.
	 */
	public static BundleContext context;
	
	@Override
	public void start(BundleContext ctx) throws Exception {
		context = ctx;
	}

	@Override
	public void stop(BundleContext ctx) throws Exception {
		if (ctx == context) {
			context = null;
		}
	}

}
