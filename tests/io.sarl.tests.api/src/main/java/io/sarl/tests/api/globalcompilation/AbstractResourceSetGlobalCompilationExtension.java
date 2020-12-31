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
package io.sarl.tests.api.globalcompilation;

import javax.inject.Inject;

import com.google.inject.Injector;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ExtensionContext.Namespace;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.ExtendedSARLInjectorProvider;
import io.sarl.tests.api.extensions.AbstractInjectorExtension;
import io.sarl.tests.api.tools.SarlValidationTestHelper;

/** Abstract JUnit5 extension for running tests within a global compilation process. 
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 */
@InjectWith(ExtendedSARLInjectorProvider.class)
public class AbstractResourceSetGlobalCompilationExtension extends AbstractInjectorExtension {

	static final Namespace NAMESPACE = Namespace.create(AbstractResourceSetGlobalCompilationExtension.class);

	static final String CONTEXT_KEY = "context";

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
		final ExtensionContext root = context.getRoot();
		return root.getStore(NAMESPACE).getOrComputeIfAbsent(
				ResourceSetGlobalCompilationExtension.CONTEXT_KEY,
				it -> {
					final ResourceSetGlobalCompilationContext ctx = new ResourceSetGlobalCompilationContext(
							context.getRequiredTestClass().getPackage().getName() + ".tests",
							this.injector, this.parseHelper, this.validator);
					final GlobalCompilationTestContribution anno = context.getRequiredTestClass().getAnnotation(GlobalCompilationTestContribution.class);
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
	void clearCompilationContext(ExtensionContext context) {
		final ExtensionContext root = context.getRoot();
		root.getStore(NAMESPACE).remove(ResourceSetGlobalCompilationExtension.CONTEXT_KEY);
	}

}