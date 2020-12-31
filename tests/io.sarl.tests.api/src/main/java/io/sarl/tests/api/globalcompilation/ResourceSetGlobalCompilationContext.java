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

import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;

import java.util.Collection;
import java.util.Map;
import java.util.TreeMap;

import com.google.common.base.Predicate;
import com.google.common.collect.Collections2;
import com.google.inject.Injector;
import org.eclipse.emf.ecore.resource.Resource;
import org.eclipse.emf.ecore.resource.ResourceSet;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.diagnostics.Severity;
import org.eclipse.xtext.resource.XtextResourceSet;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.util.Strings;
import org.eclipse.xtext.validation.Issue;
import org.eclipse.xtext.xbase.lib.Pair;
import org.eclipse.xtext.xbase.validation.IssueCodes;

import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.tools.SarlValidationTestHelper;
import io.sarl.tests.api.tools.TestUtils;

/** Context for the {@link ResourceSetGlobalCompilationUnitExtension massive compilation extension}.
 *
 * <p>The global compilation for a resource set enables to compile, i.e. generate the Java code
 * for all the source files into a given resource set in a single compilation step.
 * 
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.9
 */
public class ResourceSetGlobalCompilationContext {

	/** Default value for the flag that indicates if the validation is forced at
	 * each test method run.
	 *
	 * @since 0.11
	 */
	public static final boolean DEFAULT_VALIDATION_IN_TEST_METHODS = false;
	
	private final String basePackage;

	private final Injector injector;

	private final ParseHelper<SarlScript> parser;

	private final SarlValidationTestHelper validator;

	private final Map<String, Pair<String, String>> expectedJava = new TreeMap<>();

	private ResourceSet resourceSet;

	private boolean validateInEachTestFunction = DEFAULT_VALIDATION_IN_TEST_METHODS;

	private long index;

	private String currentMethod;

	/** Constructor.
	 *
	 * @param basePackage the name of the package that is the root for all the resources.
	 * @param injector the injector.
	 * @param parser the SARL parser.
	 * @param validator the validator.
	 */
	ResourceSetGlobalCompilationContext(String basePackage, Injector injector, ParseHelper<SarlScript> parser,
			SarlValidationTestHelper validator) {
		assert basePackage != null;
		assert injector != null;
		assert parser != null;
		assert validator != null;
		this.basePackage = basePackage;
		this.injector = injector;
		this.parser = parser;
		this.validator = validator;
	}

	/** Replies the expected java results.
	 *
	 * @return the expected results.
	 */
	public synchronized Map<String, Pair<String, String>> getExpectedResults() {
		return this.expectedJava;
	}

	/** Replies the resource set.
	 *
	 * @return the resource set.
	 */
	public synchronized ResourceSet getResourceSet() {
		return this.resourceSet;
	}

	/** Replies if the validation should be executed in each test function.
	 *
	 * @return {@code true} if the validation is run in each test function.
	 * @since 0.11
	 */
	public boolean isValidationRunInEachTestFunction() {
		return this.validateInEachTestFunction;
	}

	/** Set if the validation should be executed in each test function.
	 *
	 * @param validation {@code true} if the validation is run in each test function.
	 * @since 0.11
	 */
	void setValidationRunInEachTestFunction(boolean validation) {
		this.validateInEachTestFunction = validation;
	}

	/** Set the name of the current method.
	 *
	 * @param methodName the name of the method.
	 * @since 0.11
	 */
	void setCurrentMethod(String methodName) {
		this.currentMethod = methodName;
	}

	/** Build a unique package name.
	 *
	 * @return the package name.
	 * @since 0.11
	 */
	public String buildPackageName() {
		final long idx;
		synchronized (this) {
			idx = this.index;
			++this.index;
		}
		final StringBuilder name = new StringBuilder();
		name.append(this.basePackage);
		name.append(".test"); //$NON-NLS-1$
		name.append(idx);
		if (!Strings.isEmpty(this.currentMethod)) {
			name.append(".");
			name.append(this.currentMethod);
		}
		return name.toString();
	}

	/** Assert that a single type is correctly generated.
	 * Correctly means that the expected Java code is generated and equals to the expected code.
	 *
	 * @param sarlExpression the SARL expression to compile, without the package definition.
	 * @param javaExpression the expected Java expression, without the package definition.
	 * @throws Exception in case of error.
	 */
	public void compileTo(String sarlExpression, String javaExpression) throws Exception {
		final String packageName = buildPackageName();
		final String inputCode = "package " + packageName + TestUtils.getLineSeparator() + sarlExpression; //$NON-NLS-1$ //$NON-NLS-2$
		final SarlScript script = file(inputCode, isValidationRunInEachTestFunction(), true);
		final String qualifiedName = packageName + "." + script.getXtendTypes().get(script.getXtendTypes().size() - 1).getName(); //$NON-NLS-1$
		final String expectedJava = "package " + packageName + ";" //$NON-NLS-1$ //$NON-NLS-2$
				+ TestUtils.getLineSeparator() + TestUtils.getLineSeparator()+ javaExpression;
		this.expectedJava.put(qualifiedName, Pair.of(this.currentMethod, expectedJava));
	}

	/** Assert that a single type is correctly generated.
	 * Correctly means that the expected Java code is generated and equals to the expected code.
	 *
	 * @param sarlExpressions the SARL expressions to compile, with the package definitions.
	 * @param typeName the name of the java type to verify.
	 * @param javaExpression the expected Java expression, with the package definition.
	 * @throws Exception in case of error.
	 */
	public synchronized void compileTo(String[] sarlExpressions, String typeName, String javaExpression) throws Exception {
		SarlScript script = null;
		for (final String sarlExpression : sarlExpressions) {
			if (this.resourceSet == null) {
				script = this.parser.parse(sarlExpression);
				this.resourceSet = script.eResource().getResourceSet();
			} else {
				script = this.parser.parse(sarlExpression, this.resourceSet);
			}
		}
		assertNotNull(script);
		this.expectedJava.put(typeName, Pair.of(this.currentMethod, javaExpression));
	}

	/** Assert that a single type is not correctly generated due to invalid implementation of the Xbase API.
	 * In future version of this class, this function may disappear.
	 *
	 * @param sarlExpression the SARL expression to compile, without the package definition.
	 * @throws Exception in case of error.
	 */
	public void compileToUnexpectedCastError(String sarlExpression) throws Exception {
		final String packageName = buildPackageName();
		final String inputCode = "package " + packageName + TestUtils.getLineSeparator() + sarlExpression; //$NON-NLS-1$
		validate(this.validator, this.injector,
				file(inputCode, true, true).eResource()).assertError(
					TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
					IssueCodes.INVALID_CAST);
	}

	private synchronized SarlScript file(String code, boolean validate, boolean updateResourceSet) throws Exception {
		SarlScript script;
		if (this.resourceSet == null) {
			script = this.parser.parse(code);
			if (updateResourceSet) {
				this.resourceSet = script.eResource().getResourceSet();
			}
		} else {
			script = this.parser.parse(code, this.resourceSet);
		}
		if (this.resourceSet instanceof XtextResourceSet) {
			((XtextResourceSet) this.resourceSet).setClasspathURIContext(getClass());
		}
		Resource resource = script.eResource();
		assertEquals(0, resource.getErrors().size(), () -> resource.getErrors().toString());
		if (validate) {
			Collection<Issue> issues = Collections2.filter(this.validator.validate(resource), new Predicate<Issue>() {
				@Override
				public boolean apply(Issue input) {
					return input.getSeverity() == Severity.ERROR;
				}
			});
			assertTrue(issues.isEmpty(), () -> "Resource contained errors : " + issues.toString()); //$NON-NLS-1$
		}
		return script;
	}

}
