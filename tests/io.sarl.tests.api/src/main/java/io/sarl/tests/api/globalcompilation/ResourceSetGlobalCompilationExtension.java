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

import static io.sarl.tests.api.tools.TestAssertions.assertEqualsExceptNewLines;
import static org.junit.jupiter.api.DynamicTest.dynamicTest;

import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map.Entry;

import javax.inject.Inject;

import com.google.common.base.Throwables;
import org.eclipse.emf.common.notify.Adapter;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.resource.XtextResource;
import org.eclipse.xtext.resource.impl.ResourceDescriptionsData.ResourceSetAdapter;
import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.jupiter.api.DynamicTest;
import org.junit.jupiter.api.TestFactory;
import org.junit.jupiter.api.extension.AfterAllCallback;
import org.junit.jupiter.api.extension.BeforeAllCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

import io.sarl.tests.api.ExtendedSARLInjectorProvider;
import io.sarl.tests.api.tools.TestUtils;

/** JUnit5 extension for making a test class as including a
 * global compilation of a resource set. 
 *
 * <p>The global compilation for a resource set enables to compile, i.e. generate the Java code
 * for all the source files into a given resource set in a single compilation step.
 *
 * <p>The participating functions to the global compilation process must be annoted with
 * {@link GlobalCompilationTestContribution}.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.11
 * @see GlobalCompilationTestContribution
 */
@InjectWith(ExtendedSARLInjectorProvider.class)
class ResourceSetGlobalCompilationExtension extends AbstractResourceSetGlobalCompilationExtension implements AfterAllCallback, BeforeAllCallback {

	@Inject
	private CompilationTestHelper compiler;

	@TestFactory
	public List<DynamicTest> createDynamicTests() {
		throw new UnsupportedOperationException();
	}

	@Override
	public void beforeAll(ExtensionContext context) throws Exception {
		try {
			Class<?> type = context.getRequiredTestClass();
			if (!Modifier.isStatic(type.getModifiers())) {
				if (type.isMemberClass()) {
					throw new IllegalStateException("Member class annoted with MassiveCompilationExtension extension must be declared with static modifier.");
				}
			}
			injectMembers(this, context);
			clearCompilationContext(context);
		} catch (Throwable e) {
			e.printStackTrace();
		    Throwables.throwIfUnchecked(e);
		    throw new RuntimeException(e);
		}
	}

	@Override
	public void afterAll(ExtensionContext context) throws Exception {
		try {
			final ResourceSetGlobalCompilationContext compilationContext = getOrCreateCompilationContext(context);
			if (compilationContext != null) {
				final ResourceSet rs = compilationContext.getResourceSet();
				if (rs != null) {
					// Remove the resources that are not XtextResource because they
					// are causing issues related to their indexation (in the linking stage)
					final Iterator<Resource> resources = rs.getResources().iterator();
					while (resources.hasNext()) {
						final Resource resource = resources.next();
						if (!(resource instanceof XtextResource)) {
							resources.remove();
						}
					}
					// Remove the resource set descriptions in order to be installable during linking stage
					final Iterator<Adapter> adapters = rs.eAdapters().iterator();
					while (adapters.hasNext()) {
						final Adapter adapter = adapters.next();
						if (adapter instanceof ResourceSetAdapter) {
							adapters.remove();
						}
					}

					final List<DynamicTest> dynamicTestResults = new ArrayList<>();

					// Do the compilation
					this.compiler.compile(rs, it -> {
						for (final Entry<String, Pair<String, String>> entry : compilationContext.getExpectedResults().entrySet()) {
							final String id = entry.getKey();
							final String actual = it.getGeneratedCode(entry.getKey());
							final String functionName = entry.getValue().getKey();
							final String expected = entry.getValue().getValue();
							final DynamicTest test = dynamicTest("Java compilation - " + functionName, () -> {
								assertEqualsExceptNewLines(expected, actual, () -> {
									final String diff = TestUtils.differences(expected, actual);
									if (!Strings.isEmpty(functionName)) {
										return functionName + ", RAW DIFF = " + diff;
									}
									final int index1 = id.lastIndexOf('.');
									final int index0 = id.lastIndexOf('.', index1 - 1) + 1;
									return entry.getKey().substring(index0, index1) + ", RAW DIFF = " + diff;
								});
							});
							dynamicTestResults.add(test);
						}
					});

					// Generate the tests results
					for(final DynamicTest test : dynamicTestResults) {
						test.getExecutable().execute();
					}
				}
			} else {
				throw new IllegalStateException("no massive compilation context found");
			}
		} catch (Throwable e) {
			e.printStackTrace();
		    Throwables.throwIfUnchecked(e);
		    throw new RuntimeException(e);
		}
	}

}
