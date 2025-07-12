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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.sarl.bspl.lang.tests;

import static io.sarl.tests.api.tools.TestEObjects.fileGen;
import static io.sarl.tests.api.tools.TestUtils.multilineString;

import java.io.IOException;
import java.util.regex.Pattern;

import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Assert;
import org.junit.jupiter.api.extension.ExtendWith;

import com.google.inject.Inject;
import com.google.inject.Injector;

import io.sarl.bspl.lang.bspl.BsplProtocolSpecification;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.FieldResetExtension;
import io.sarl.tests.api.extensions.IgnorableTestExtension;
import io.sarl.tests.api.extensions.MockInitializerExtension;
import io.sarl.tests.api.extensions.SarlInjectionExtension;
import io.sarl.tests.api.tools.SarlValidationTestHelper;
import io.sarl.tests.api.tools.TestValidator;
import io.sarl.tests.api.tools.TestValidator.Validator;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
@ExtendWith({
	ContextInitExtension.class, JavaVersionCheckExtension.class, 
	SarlInjectionExtension.class, MockInitializerExtension.class, 
	IgnorableTestExtension.class, FieldResetExtension.class})
@InjectWith(ExtendedBSPLInjectorProvider.class)
public abstract class AbstractBsplTest {

	@Inject
	private Injector injector;

	@Inject
	private ParseHelper<BsplProtocolSpecification> specificationParser;

	@Inject
	private ExtendedCompilationTestHelper compiler;

	@Inject
	private SarlValidationTestHelper validationHelper;

	@Override
	protected void finalize() throws Throwable {
		this.injector = null;
		this.compiler = null;
		this.specificationParser = null;
		this.validationHelper = null;
	}

	/** Replies the injector.
	 *
	 * @return the injector.
	 * @since 0.10
	 */
	protected Injector getInjector() {
		return this.injector;
	}

	/** Parse the code and replies the SARL BSPL specification. This function does not validate the code.
	 *
	 * @param code the code to parse
	 * @return the specification.
	 * @throws Exception in the case the code cannot be parse properly.
	 */
	protected BsplProtocolSpecification specification(String... code) throws Exception {
		return fileGen(this.specificationParser, multilineString(code));
		
	}

	/** Parse the code and replies the SARL BSPL specification. This function validates the code.
	 *
	 * @param code the code to parse
	 * @return the specification.
	 * @throws Exception in the case the code cannot be parse properly.
	 */
	protected BsplProtocolSpecification specificationValid(String... code) throws Exception {
		return fileGen(this.specificationParser, getValidationHelper(), multilineString(code));
		
	}

	/** Validate the given specification.
	 *
	 * @param specification the specification.
	 * @return the validation result accessor.
	 */
	protected Validator validate(BsplProtocolSpecification specification) {
		final var validationHelper = getValidationHelper();
		final var injector = getInjector();
		final var xtextResource = specification.eResource();
		return TestValidator.validate(validationHelper, injector, xtextResource);
	}

	/** Replies the compile helper.
	 *
	 * @return the compile helper.
	 */
	protected ExtendedCompilationTestHelper getCompileHelper() {
		return this.compiler;
	}

	/** Replies the validation helper.
	 *
	 * @return the validation helper.
	 */
	protected SarlValidationTestHelper getValidationHelper() {
		return this.validationHelper;
	}
	
	/**
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	public static class ExtendedCompilationTestHelper extends CompilationTestHelper {

		/** Constructor.
		 */
		public ExtendedCompilationTestHelper() {
			//
		}
		
		/**
		 * Asserts that the expected code in the given file is generated for the given source.
		 * 
		 * @param source some valid source code written in the language under test
		 * @param typename the name of the generated type. 
		 * @param expected the expected Java source code.
		 * @throws IOException if the resource loading fails 
		 */
		public void assertCompilesTo(CharSequence source, String typename, CharSequence expected) throws IOException {
			final boolean[] called = {false};
			compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					var generatedCode = r.getGeneratedCode(typename);
					if (generatedCode == null) {
						// Check for generated code that is not Java-based
						final var pattern = Pattern.compile(Pattern.quote(typename.replaceAll(Pattern.quote("."), "/")) + "(\\.[^.]+)?$");
						final var allResources = r.getAllGeneratedResources().entrySet();
						final var genResource = allResources.stream().filter(it -> pattern.matcher(it.getKey()).find()).findFirst();
						if (genResource.isPresent()) {
							final var value = genResource.get().getValue();
							if (value != null) {
								generatedCode = value.toString();
							}
						}
					}
					Assert.assertEquals(expected.toString(), generatedCode);
					called[0] = true;
				}
			});
			Assert.assertTrue("Nothing was generated but the expectation was :\n" + expected, called[0]);
		}

		

	}

}
