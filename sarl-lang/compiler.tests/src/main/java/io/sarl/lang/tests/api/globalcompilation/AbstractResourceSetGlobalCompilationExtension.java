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
package io.sarl.lang.tests.api.globalcompilation;

import com.google.inject.Inject;
import com.google.inject.Injector;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.ExtendedSARLInjectorProvider;
import io.sarl.tests.api.extensions.AbstractInjectorExtension;
import io.sarl.tests.api.tools.SarlValidationTestHelper;

/** Abstract JUnit5 extension for running tests within a global compilation process. 
 *
 * @author $Author: sgalland$
 * @version compiler.tests 0.15.0 20250909-115746
 * @mavengroupid io.sarl.lang
 * @mavenartifactid compiler.tests
 * @since 0.11
 */
@InjectWith(ExtendedSARLInjectorProvider.class)
public class AbstractResourceSetGlobalCompilationExtension extends AbstractInjectorExtension {

	/** The namespace.
	 */
	static final Namespace NAMESPACE = Namespace.create(AbstractResourceSetGlobalCompilationExtension.class);

	/** The context key.
	 */
	static final String CONTEXT_KEY = "context"; //$NON-NLS-1$

	@Inject
	private ParseHelper<SarlScript> parseHelper;
	
	@Inject
	private Injector injector;

	@Inject
	private SarlValidationTestHelper validator;

	/** Get the compilation context.
	 *
	 * @param context the extension context.
	 * @return the compilation context.
	 */
	ResourceSetGlobalCompilationContext getOrCreateCompilationContext(ExtensionContext context) {
		final var root = context.getRoot();
		return root.getStore(NAMESPACE).getOrComputeIfAbsent(
				AbstractResourceSetGlobalCompilationExtension.CONTEXT_KEY,
				it -> {
					final var ctx = new ResourceSetGlobalCompilationContext(
							context.getRequiredTestClass().getPackage().getName() + ".tests", //$NON-NLS-1$
							this.injector, this.parseHelper, this.validator);
					final var anno = context.getRequiredTestClass().getAnnotation(GlobalCompilationTestContribution.class);
					if (anno != null) {
						ctx.setValidationRunInEachTestFunction(anno.getValidate());
					}
					return ctx;
				},
				ResourceSetGlobalCompilationContext.class);
	}

	/** Remove the compilation context.
	 *
	 * @param context the extension context.
	 */
	@SuppressWarnings("static-method")
	void clearCompilationContext(ExtensionContext context) {
		final var root = context.getRoot();
		root.getStore(NAMESPACE).remove(AbstractResourceSetGlobalCompilationExtension.CONTEXT_KEY);
	}

}