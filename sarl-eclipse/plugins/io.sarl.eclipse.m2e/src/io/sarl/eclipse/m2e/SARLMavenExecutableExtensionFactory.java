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

package io.sarl.eclipse.m2e;

import com.google.inject.Injector;
import org.osgi.framework.Bundle;

import io.sarl.eclipse.SARLEclipseExecutableExtensionFactory;
import io.sarl.lang.ui.internal.LangActivator;

/** Factory for injecting SARL instances.
 *
 * @author $Author: sgalland$
 * @version io.sarl.eclipse.m2e 0.15.0 20250909-115751
 * @mavengroupid io.sarl.eclipse
 * @mavenartifactid io.sarl.eclipse.m2e
 */
public class SARLMavenExecutableExtensionFactory extends SARLEclipseExecutableExtensionFactory {

	@Override
	protected Bundle getBundle() {
		return SARLMavenEclipsePlugin.getDefault().getBundle();
	}

	/** Replies the SARL injector.
	 *
	 * @return the injector.
	 */
	public static Injector getSARLInjector() {
		return LangActivator.getInstance().getInjector(LangActivator.IO_SARL_LANG_SARL);
	}

}
