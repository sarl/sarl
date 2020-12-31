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
package io.sarl.tests.api.extensions;

import org.eclipse.xtext.testing.extensions.InjectionExtension;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.TestInstancePostProcessor;

/** JUnit 5 extension that injects the SARL components.
 *
 * <p>This implementation does not inject the elements at the same step of the JUnit 5 life-cycle
 * as the standard Xtext {@link InjectionExtension}. Moreover, this extension does
 * a restore of the injection provider in order to avoid unstable state of the injected components.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @see InjectionExtension
 */
public class SarlInjectionExtension extends AbstractInjectorExtension implements TestInstancePostProcessor {
	
	@Override
	public void postProcessTestInstance(Object testInstance, ExtensionContext context) throws Exception {
		injectMembers(testInstance, context);
	}
	
}
