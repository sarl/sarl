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

import static org.junit.platform.commons.util.AnnotationUtils.isAnnotated;
import static org.junit.platform.commons.util.ReflectionUtils.isStatic;

import java.lang.reflect.Method;
import java.util.Collections;
import java.util.List;
import java.util.stream.Stream;

import org.eclipse.xtext.testing.InjectWith;
import org.junit.jupiter.api.extension.Extension;
import org.junit.jupiter.api.extension.ExtensionContext;
import org.junit.jupiter.api.extension.ParameterContext;
import org.junit.jupiter.api.extension.ParameterResolutionException;
import org.junit.jupiter.api.extension.ParameterResolver;
import org.junit.jupiter.api.extension.TestTemplateInvocationContext;
import org.junit.jupiter.api.extension.TestTemplateInvocationContextProvider;

import io.sarl.tests.api.ExtendedSARLInjectorProvider;

/** JUnit5 extension for making a function as a participant to the
 * global compilation of a resource set. 
 *
 * <p>The global compilation for a resource set enables to compile, i.e. generate the Java code
 * for all the source files into a given resource set in a single compilation step.
 *
 * <p>This extension should not directly be used. It is automatically invoked by the
 * {@link GlobalCompilationTestContribution} annotation.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @see GlobalCompilationTestContribution
 */
@InjectWith(ExtendedSARLInjectorProvider.class)
class ResourceSetGlobalCompilationUnitExtension extends AbstractResourceSetGlobalCompilationExtension implements TestTemplateInvocationContextProvider {

	@Override
	public boolean supportsTestTemplate(ExtensionContext context) {
		if (isAnnotated(context.getTestMethod(), GlobalCompilationTestContribution.class)) {
			final Method meth = context.getTestMethod().get();
			if (isStatic(meth)) {
				throw new IllegalStateException("The massive compilation function must not have the static modifier");
			}
			if (meth.getParameterCount() != 1
					|| !meth.getParameters()[0].getType().isAssignableFrom(ResourceSetGlobalCompilationContext.class)) {
				throw new IllegalStateException("Invalid parameters for the massive compilation function");
			}
			// Ensure the compilation context is alive.
			try {
				injectMembers(this, context);
			} catch (Exception ex) {
				throw new IllegalStateException(ex);
			}
			getOrCreateCompilationContext(context);
			return true;
		}
		return false;
	}

	@Override
	public Stream<TestTemplateInvocationContext> provideTestTemplateInvocationContexts(ExtensionContext context) {
		final ResourceSetGlobalCompilationContext ctx = getOrCreateCompilationContext(context);
		final Method meth = context.getRequiredTestMethod();
		ctx.setCurrentMethod(meth.getName());
		return Stream.of(new DynamicContext(meth.getName()));
	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	private class DynamicParameterResolver implements ParameterResolver {

		@Override
		public boolean supportsParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
				throws ParameterResolutionException {
			return ResourceSetGlobalCompilationContext.class.isAssignableFrom(parameterContext.getParameter().getType());
		}

		@Override
		public Object resolveParameter(ParameterContext parameterContext, ExtensionContext extensionContext)
				throws ParameterResolutionException {
			return getOrCreateCompilationContext(extensionContext);
		}

	}

	/**
	 * @author $Author: sgalland$
	 * @version $FullVersion$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @since 0.11
	 */
	private class DynamicContext implements TestTemplateInvocationContext {

		private final String name;

		/** Constructor.
		 * 
		 * @param name the display name.
		 */
		DynamicContext(String name) {
			this.name = name;
		}

		@Override
		public String getDisplayName(int invocationIndex) {
			return this.name;
		}

		@Override
		public List<Extension> getAdditionalExtensions() {
			return Collections.singletonList(new DynamicParameterResolver());
		}

	}

}
