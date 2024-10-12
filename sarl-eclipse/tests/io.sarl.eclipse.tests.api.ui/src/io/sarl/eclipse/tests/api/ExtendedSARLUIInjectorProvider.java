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
package io.sarl.eclipse.tests.api;

import static org.mockito.Mockito.*;

import com.google.inject.Injector;
import org.osgi.framework.Bundle;
import org.osgi.framework.BundleContext;

import io.sarl.lang.ui.internal.LangActivator;
import io.sarl.lang.ui.tests.SARLUiInjectorProvider;

/** Override the module definition for tests only.
 *
 * <p>This class is implemented for overriding the default Java version of the on-the-fly Java compiler
 * that is used by the testing framework. Indeed, the default Java version for this compiler is Java 6. But,
 * we are expecting another version, as described in {@link SARLVersion}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.13
 */
public class ExtendedSARLUIInjectorProvider extends SARLUiInjectorProvider {

	@Override
	public Injector getInjector() {
		if (LangActivator.getInstance() == null) {
			final Bundle bundle = mock(Bundle.class); 
			final BundleContext context = mock(BundleContext.class);
			when(context.getBundle()).thenReturn(bundle);
			when(bundle.getBundleContext()).thenReturn(context);
			final LangActivator activator = new LangActivator();
			try {
				activator.start(context);
			} catch (Exception ex) {
				throw new RuntimeException(ex);
			}
		}
		return super.getInjector();
	}

}
