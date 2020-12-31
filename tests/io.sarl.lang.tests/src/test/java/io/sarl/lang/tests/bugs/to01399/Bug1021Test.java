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
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sarl.lang.tests.bugs.to01399;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.junit.jupiter.api.Assertions.assertEquals;

import org.eclipse.xtend.core.xtend.XtendPackage;
import org.eclipse.xtext.common.types.TypesPackage;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Invalid declaration of generic types for functions.
 *
 * <p>https://github.com/sarl/sarl/issues/1021
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/sarl/sarl/issues/1021"
 */
@DisplayName("Bug #1021")
@SuppressWarnings("all")
@Tag("core")
@Tag("sarlParsing")
public class Bug1021Test {

	/** Inner class.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see "https://github.com/sarl/sarl/issues/1021"
	 */
	@Nested
	@DisplayName("Inner generic type")
	public class InnerClass extends AbstractSarlTest  {
	
		private final String SARL_CODE_01 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner<T> {",
				"  }",
				"}");

		@Test
		@DisplayName("Single generic type")
		public void parsing01() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_01);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_02 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<TT, T> {",
				"  static class Inner<T> {",
				"  }",
				"}");

		@Test
		@DisplayName("Two generic types")
		public void parsing02() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_02);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator
				.assertNoIssues();
		}

	}

	/** Inner generic class, Inner-Inner class.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see "https://github.com/sarl/sarl/issues/1021"
	 */
	@Nested
	public class InnerGenericClassInnerClass extends AbstractSarlTest  {
	
		private final String SARL_CODE_01 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1 {",
				"  static class Inner<T> {",
				"    static class Inner2<T> {",
				"    }",
				"  }",
				"}");

		@Test
		@DisplayName("Single generic type")
		public void parsing01() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_01);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

	}

	/** Inner not-generic class, Inner-Inner class.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see "https://github.com/sarl/sarl/issues/1021"
	 */
	@Nested
	public class InnerNotGenericClassInnerClass extends AbstractSarlTest  {
	
		private final String SARL_CODE_01 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1 {",
				"  static class Inner {",
				"    static class Inner2<T> {",
				"    }",
				"  }",
				"}");


		@Test
		@DisplayName("No generic type")
		public void parsing01() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_01);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

	}

	/** Functions.
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 * @see "https://github.com/sarl/sarl/issues/1021"
	 */
	@Nested
	public class Functions extends AbstractSarlTest  {
	
		private final String SARL_CODE_01 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  def fct : void with T {",
				"  }",
				"}");


		@Test
		@DisplayName("Hiding with SARL notation")
		public void parsing01() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_01);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					TypesPackage.eINSTANCE.getJvmTypeParameter(),
					IssueCodes.GENERIC_TYPE_NAME_SHADOWING,
					"'T'", "is hiding", "'Bug1021Type1'");
		}

		private final String SARL_CODE_02 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  def <T> fct : void {",
				"  }",
				"}");

		@Test
		@DisplayName("Hiding with Java notation")
		public void parsing02() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_02);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					TypesPackage.eINSTANCE.getJvmTypeParameter(),
					IssueCodes.GENERIC_TYPE_NAME_SHADOWING,
					"'T'", "is hiding", "'Bug1021Type1'");
		}

		private final String SARL_CODE_03 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner {",
				"    def fct : void with T {",
				"    }",
				"  }",
				"}");
	
		@Test
		@DisplayName("No generic inner with SARL notation")
		public void parsing03() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_03);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_04 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner {",
				"    def <T> fct : void {",
				"    }",
				"  }",
				"}");


		@Test
		@DisplayName("No generic inner with Java notation")
		public void parsing04() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_04);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_05 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner<TT> {",
				"    def fct : void with T {",
				"    }",
				"  }",
				"}");
	

		@Test
		@DisplayName("No inner hiding with SARL notation")
		public void parsing05() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_05);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_06 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner<TT> {",
				"    def <T> fct : void {",
				"    }",
				"  }",
				"}");

		@Test
		@DisplayName("No inner hiding with Java notation")
		public void parsing06() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_06);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		private final String SARL_CODE_07 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner<T> {",
				"    def fct : void with T {",
				"    }",
				"  }",
				"}");
	
		@Test
		@DisplayName("Inner hiding with SARL notation")
		public void parsing07() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_07);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					TypesPackage.eINSTANCE.getJvmTypeParameter(),
					IssueCodes.GENERIC_TYPE_NAME_SHADOWING,
					"'T'", "is hiding", "'Inner'");
		}

		private final String SARL_CODE_08 = multilineString(
				"package io.sarl.lang.tests.bug1021",
				"class Bug1021Type1<T> {",
				"  static class Inner<T> {",
				"    def <T> fct : void {",
				"    }",
				"  }",
				"}");

		@Test
		@DisplayName("Inner hiding with Java notation")
		public void parsing08() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CODE_08);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					TypesPackage.eINSTANCE.getJvmTypeParameter(),
					IssueCodes.GENERIC_TYPE_NAME_SHADOWING,
					"'T'", "is hiding", "'Inner'");
		}

	}

}
