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

import org.eclipse.xtext.testing.InjectWith;
import org.eclipse.xtext.testing.util.ParseHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.jupiter.api.extension.ExtendWith;

import com.google.inject.Inject;

import io.sarl.bspl.lang.sarl_bspl.BsplProtocolSpecification;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.tests.api.extensions.JavaVersionCheckExtension;
import io.sarl.tests.api.extensions.ContextInitExtension;
import io.sarl.tests.api.extensions.FieldResetExtension;
import io.sarl.tests.api.extensions.IgnorableTestExtension;
import io.sarl.tests.api.extensions.MockInitializerExtension;
import io.sarl.tests.api.extensions.SarlInjectionExtension;
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
@InjectWith(ExtendedSARL_BSPLInjectorProvider.class)
public abstract class AbstractSarlBsplTest extends AbstractSarlTest {

	@Inject
	private ParseHelper<BsplProtocolSpecification> specificationParser;

	@Inject
	private CompilationTestHelper compiler;

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
	protected CompilationTestHelper getBsplCompileHelper() {
		return this.compiler;
	}

}
