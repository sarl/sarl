/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.lang.ui.tests;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.xtext.testing.InjectWith;
import org.junit.jupiter.api.extension.ExtendWith;

import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.FieldResetExtension;
import io.sarl.tests.api.extensions.IgnorableTestExtension;
import io.sarl.tests.api.extensions.MockInitializerExtension;
import io.sarl.tests.api.extensions.SarlInjectionExtension;

/** Abstract implementation of a test for UI.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.15
 */
@SuppressWarnings("all")
@ExtendWith({
	ContextInitExtension.class, SarlInjectionExtension.class,
	MockInitializerExtension.class, IgnorableTestExtension.class, FieldResetExtension.class})
@InjectWith(ExtendedSARLUIInjectorProvider.class)
public abstract class AbstractSarlUiTest {

	@Inject
	private Injector injector;

	/** Replies the injector.
	 *
	 * @return the injector.
	 */
	protected Injector getInjector() {
		return this.injector;
	}

}
