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

package io.sarl.lang.tests.bugs.to00699;

import static io.sarl.lang.validation.IssueCodes.ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION;
import static io.sarl.lang.validation.IssueCodes.PARAMETER_DEFAULT_VALUE_REDFINITION;
import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;
import static org.eclipse.xtext.xbase.validation.IssueCodes.FORBIDDEN_REFERENCE;
import static org.eclipse.xtext.xbase.validation.IssueCodes.STATIC_ACCESS_TO_INSTANCE_MEMBER;

import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.core.AgentContext;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.tools.TestValidator.Validator;

/** Testing class for issue: Enable the use of instance-related
 * variables or functions within parameters default value.
 *
 * <p>https://github.com/sarl/sarl/issues/612
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see https://github.com/sarl/sarl/issues/612
 */
@DisplayName("Bug #612")
@SuppressWarnings("all")
@Tag("core")
public class Bug612Test {

	/** The container is a class. 
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Class Container")
	@Nested
	public class ClassContainer {

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from function")
		@Nested
		public class ConstantReference extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def fct(param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static field. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static field reference from function")
		@Nested
		public class StaticFieldReference extends AbstractSarlTest {

			private final String LOCAL_STATIC_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var SVAL : int = 5",
					"  def fct(param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Local static field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field compiling")
			public void compiling_localStaticField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  def fct(param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Local static final field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFinalField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static final field compiling")
			public void compiling_localStaticFinalField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  def fct(param : int = SVAL + 7) { }",
					"}");

			@Test
			@DisplayName("Local static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field expression compiling")
			public void compiling_localStaticFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL + 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.SVAL + 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from function")
		@Nested
		public class StaticFunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"class XXX {",
					"  @Pure static def pureFunction : int {5}",
					"  def fct(param : int = pureFunction) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var field : int",
					"  static def getX : int {field}",
					"  static def notPureFunction : int {field = 5; return 5}",
					"  def fct(param : int = notPureFunction) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: notPureFunction() : int");
			}

		}

		/** Referencing an instance field.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Field reference from function")
		@Nested
		public class FieldReference extends AbstractSarlTest {

			private final String THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("this.field parsing")
			@Tag("sarlValidation")
			public void parsing_thisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("this.field compiling")
			public void compiling_thisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Field parsing without this")
			@Tag("sarlValidation")
			public void parsing_fieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Field compiling without this")
			public void compiling_fieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  val field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("Final this.field parsing")
			@Tag("sarlValidation")
			public void parsing_finalThisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final this.field compiling")
			public void compiling_finalThisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  val field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Final field parsing without this")
			@Tag("sarlValidation")
			public void parsing_finalFieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final field compiling without this")
			public void compiling_finalFieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling")
			public void compiling_fieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String THIS_EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing with this")
			@Tag("sarlValidation")
			public void parsing_thisFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling with this")
			public void compiling_thisFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from function")
		@Nested
		public class FunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def setX : int {5}",
					"  def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX * 7) { }",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  var field : int",
					"  def getX : int {return this.field}",
					"  def setX(a : int) : int {this.field = a; return a}",
					"  def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from static function")
		@Nested
		public class ConstantReferenceStatic extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def fct(param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static field. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static field reference from static function")
		@Nested
		public class StaticFieldReferenceStatic extends AbstractSarlTest {

			private final String LOCAL_STATIC_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var SVAL : int = 5",
					"  static def fct(param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Local static field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field compiling")
			public void compiling_localStaticField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  static def fct(param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Local static final field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFinalField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static final field compiling")
			public void compiling_localStaticFinalField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  static def fct(param : int = SVAL + 7) { }",
					"}");

			@Test
			@DisplayName("Local static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field expression compiling")
			public void compiling_localStaticFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL + 7\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.SVAL + 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from static function")
		@Nested
		public class StaticFunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"class XXX {",
					"  @Pure static def pureFunction : int {5}",
					"  static def fct(param : int = pureFunction) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var field : int",
					"  static def getX : int {field}",
					"  static def notPureFunction : int {field = 5; return 5}",
					"  static def fct(param : int = notPureFunction) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: notPureFunction() : int");
			}

		}

		/** Referencing an instance field.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Field reference from static function")
		@Nested
		public class FieldReferenceStatic extends AbstractSarlTest {

			private final String FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var field : int = 5",
					"  static def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Field parsing")
			@Tag("sarlValidation")
			public void parsing_fieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Field compiling")
			public void compiling_fieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val field : int = 5",
					"  static def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Final field parsing")
			@Tag("sarlValidation")
			public void parsing_finalFieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final field compiling")
			public void compiling_finalFieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var field : int = 5",
					"  static def fct(param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling")
			public void compiling_fieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field * 7\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from static function")
		@Nested
		public class FunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def getX : int {5}",
					"  static def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public static int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = XXX.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def setX : int {5}",
					"  static def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX * 7) { }",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX() {",
						"    super();",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var field : int",
					"  static def getX : int {return field}",
					"  static def setX(a : int) : int {field = a; return a}",
					"  static def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

		/** Referencing a constant from a constructor.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from constructor")
		@Nested
		public class ConstantReferenceConstructor extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  new (param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  new (param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  val field : int = 5",
					"  new (param : int = field) { }",
					"}");

			@Test
			@DisplayName("Instance field parsing")
			@Tag("sarlValidation")
			public void parsing_field() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

			private final String FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  val field : int = 5",
					"  new (param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Instance field expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

			private final String STATIC_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  new (param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Final static field parsing")
			@Tag("sarlValidation")
			public void parsing_finalStaticField() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final static field compiling")
			public void compiling_finalStaticField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String STATIC_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var SVAL : int = 5",
					"  new (param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Static field parsing")
			@Tag("sarlValidation")
			public void parsing_staticField() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Forbidden reference to not final field SVAL from a constructors default value expression");
			}

			private final String STATIC_FINAL_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static val SVAL : int = 5",
					"  new (param : int = SVAL * 7) { }",
					"}");

			@Test
			@DisplayName("Final static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_finalStaticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FINAL_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final static field expression compiling")
			public void compiling_finalStaticFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FINAL_FIELD_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  private static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL * 7\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return (XXX.SVAL * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String STATIC_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static var SVAL : int = 5",
					"  new (param : int = SVAL * 7) { }",
					"}");

			@Test
			@DisplayName("Static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_staticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Forbidden reference to not final field SVAL from a constructors default value expression");
			}

			private final String STATIC_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def sval : int {5}",
					"  new (param : int = sval) { }",
					"}");

			@Test
			@DisplayName("Static function parsing")
			@Tag("sarlValidation")
			public void parsing_staticFct() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}


			@Test
			@Tag("compileToJava")
			@DisplayName("Static function compiling")
			public void compiling_staticFct() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public static int sval() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    int _sval = XXX.sval();",
						"    return _sval;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String STATIC_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"class XXX {",
					"  static def sval : int {5}",
					"  new (param : int = sval * 7) { }",
					"}");

			@Test
			@DisplayName("Static function expression parsing")
			@Tag("sarlValidation")
			public void parsing_staticFctExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Static function expression compiling")
			public void compiling_staticFctExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX {",
						"  @Pure",
						"  public static int sval() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval * 7\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    int _sval = XXX.sval();",
						"    return (_sval * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

		}

	}

	/** The container is an interface. 
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Interface Container")
	@Nested
	public class InterfaceContainer {

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from function")
		@Nested
		public class ConstantReference extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def fct(param : int = 5)",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def fct(param : int = 5 * 6 + 5)",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static field. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static field reference from function")
		@Nested
		public class StaticFieldReference extends AbstractSarlTest {

			private final String LOCAL_STATIC_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val SVAL : int = 5",
					"  def fct(param : int = SVAL)",
					"}");

			@Test
			@DisplayName("Local static final field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFinalField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static final field compiling")
			public void compiling_localStaticFinalField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val SVAL : int = 5",
					"  def fct(param : int = SVAL + 7)",
					"}");

			@Test
			@DisplayName("Local static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field expression compiling")
			public void compiling_localStaticFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL + 7\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.SVAL + 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from function")
		@Nested
		public class StaticFunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"interface XXX {",
					"  @Pure static def pureFunction : int { 5 }",
					"  def fct(param : int = pureFunction)",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def set(v : int) { }",
					"  static def notPureFunction : int { set(5); return 5 }",
					"  def fct(param : int = notPureFunction)",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: notPureFunction() : int");
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from function")
		@Nested
		public class FunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def getX : int",
					"  def fct(param : int = getX)",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  int getX();",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def setX : int {5}",
					"  def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def getX : int",
					"  def fct(param : int = getX * 7)",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  int getX();",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  var field : int",
					"  def getX : int {return this.field}",
					"  def setX(a : int) : int {this.field = a; return a}",
					"  def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from static function")
		@Nested
		public class ConstantReferenceStatic extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def fct(param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static field. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static field reference from static function")
		@Nested
		public class StaticFieldReferenceStatic extends AbstractSarlTest {

			private final String LOCAL_STATIC_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val SVAL : int = 5",
					"  static def fct(param : int = SVAL) { }",
					"}");

			@Test
			@DisplayName("Local static final field parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFinalField() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static final field compiling")
			public void compiling_localStaticFinalField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.SVAL;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String LOCAL_STATIC_FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val SVAL : int = 5",
					"  static def fct(param : int = SVAL + 7) { }",
					"}");

			@Test
			@DisplayName("Local static field expression parsing")
			@Tag("sarlValidation")
			public void parsing_localStaticFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), LOCAL_STATIC_FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Local static field expression compiling")
			public void compiling_localStaticFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", LOCAL_STATIC_FIELD_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int SVAL = 5;",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL + 7\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.SVAL + 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from static function")
		@Nested
		public class StaticFunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"interface XXX {",
					"  @Pure static def pureFunction : int {5}",
					"  static def fct(param : int = pureFunction) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def set(v : int) {}",
					"  static def notPureFunction : int {set(5); return 5}",
					"  static def fct(param : int = notPureFunction) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: notPureFunction() : int");
			}

		}

		/** Referencing an instance field.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Field reference from static function")
		@Nested
		public class FieldReferenceStatic extends AbstractSarlTest {

			private final String FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val field : int = 5",
					"  static def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Field parsing")
			@Tag("sarlValidation")
			public void parsing_fieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Field compiling")
			public void compiling_fieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return XXX.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static val field : int = 5",
					"  static def fct(param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling")
			public void compiling_fieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  static final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field * 7\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    return (XXX.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from static function")
		@Nested
		public class FunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def getX : int {5}",
					"  static def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  static int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  static int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = XXX.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static def setX : int {5}",
					"  static def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  def getX : int",
					"  def fct(param : int = getX * 7)",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_INTERFACE + ")",
						"@SuppressWarnings(\"all\")",
						"public interface XXX {",
						"  @Pure",
						"  int getX();",
						"  ",
						"  @DefaultValueSource",
						"  void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param);",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  default int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  default void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"interface XXX {",
					"  static var field : int",
					"  static def getX : int {return field}",
					"  static def setX(a : int) : int {field = a; return a}",
					"  static def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

	}

	/** The container is an agent. 
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Agent Container")
	@Nested
	public class AgentContainer {

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from function")
		@Nested
		public class ConstantReference extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def fct(param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from function")
		@Nested
		public class StaticFunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"agent XXX {",
					"  @Pure static def pureFunction : int {5}",
					"  def fct(param : int = pureFunction) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance field.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Field reference from function")
		@Nested
		public class FieldReference extends AbstractSarlTest {

			private final String THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("this.field parsing")
			@Tag("sarlValidation")
			public void parsing_thisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("this.field compiling")
			public void compiling_thisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Field parsing without this")
			@Tag("sarlValidation")
			public void parsing_fieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Field compiling without this")
			public void compiling_fieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  val field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("Final this.field parsing")
			@Tag("sarlValidation")
			public void parsing_finalThisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final this.field compiling")
			public void compiling_finalThisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  val field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Final field parsing without this")
			@Tag("sarlValidation")
			public void parsing_finalFieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final field compiling without this")
			public void compiling_finalFieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling")
			public void compiling_fieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String THIS_EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing with this")
			@Tag("sarlValidation")
			public void parsing_thisFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling with this")
			public void compiling_thisFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from function")
		@Nested
		public class FunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def setX : int {5}",
					"  def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX * 7) { }",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  var field : int",
					"  def getX : int {return this.field}",
					"  def setX(a : int) : int {this.field = a; return a}",
					"  def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from static function")
		@Nested
		public class ConstantReferenceStatic extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def fct(param : int = 5) { }",
					"}");

			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  protected static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  protected static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing a static function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Static function reference from static function")
		@Nested
		public class StaticFunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"import org.eclipse.xtext.xbase.lib.Pure",
					"agent XXX {",
					"  @Pure static def pureFunction : int {5}",
					"  static def fct(param : int = pureFunction) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected static int pureFunction() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"pureFunction\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    int _pureFunction = XXX.pureFunction();",
						"    return _pureFunction;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def set(v : Object) { }",
					"  static def notPureFunction : int {set(5); return 5}",
					"  static def fct(param : int = notPureFunction) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: notPureFunction() : int");
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from static function")
		@Nested
		public class FunctionReferenceStatic extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def getX : int {5}",
					"  static def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected static int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected static void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  private static int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = XXX.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def setX : int {5}",
					"  static def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX * 7) { }",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.DynamicSkillProvider;",
						"import java.util.UUID;",
						"import javax.inject.Inject;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  protected void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  protected final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final UUID arg0, final UUID arg1) {",
						"    super(arg0, arg1);",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  @Inject",
						"  public XXX(final UUID arg0, final UUID arg1, final DynamicSkillProvider arg2) {",
						"    super(arg0, arg1, arg2);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def set(v : int) {}",
					"  static def setX : int {set(5); return 5}",
					"  static def fct(param : int = setX * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX() : int");
			}

		}

		/** Referencing a constant from a constructor.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from constructor")
		@Nested
		public class ConstantReferenceConstructor extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  new (param : int = 5) { super(null, null) }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null, null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  new (param : int = 5 * 6 + 5) { super(null, null) }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null, null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  val field : int = 5",
					"  new (param : int = field) { super(null, null) }",
					"}");

			@Test
			@DisplayName("Instance field parsing")
			@Tag("sarlValidation")
			public void parsing_field() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

			private final String FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  val field : int = 5",
					"  new (param : int = field * 7) { super(null, null) }",
					"}");

			@Test
			@DisplayName("Instance field expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

			private final String STATIC_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def sval : int {5}",
					"  new (param : int = sval) { super(null, null) }",
					"}");

			@Test
			@DisplayName("Static function parsing")
			@Tag("sarlValidation")
			public void parsing_staticFct() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Static function compiling")
			public void compiling_staticFct() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected static int sval() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null, null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    int _sval = XXX.sval();",
						"    return _sval;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String STATIC_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"agent XXX {",
					"  static def sval : int {5}",
					"  new (param : int = sval * 7) { super(null, null) }",
					"}");

			@Test
			@DisplayName("Static function expression parsing")
			@Tag("sarlValidation")
			public void parsing_staticFctExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), STATIC_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Static function expression compiling")
			public void compiling_staticFctExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", STATIC_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_AGENT + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Agent {",
						"  @Pure",
						"  protected static int sval() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null, null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval * 7\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    int _sval = XXX.sval();",
						"    return (_sval * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

		}

	}

	/** The container is a behavior. 
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Behavior Container")
	@Nested
	public class BehaviorContainer {

		/** Referencing a constant. 
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Constant reference from function")
		@Nested
		public class ConstantReference extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  def fct(param : int = 5) { }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  def fct(param : int = 5 * 6 + 5) { }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance field.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Field reference from function")
		@Nested
		public class FieldReference extends AbstractSarlTest {

			private final String THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("this.field parsing")
			@Tag("sarlValidation")
			public void parsing_thisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("this.field compiling")
			public void compiling_thisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Field parsing without this")
			@Tag("sarlValidation")
			public void parsing_fieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Field compiling without this")
			public void compiling_fieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  val field : int = 5",
					"  def fct(param : int = this.field) { }",
					"}");

			@Test
			@DisplayName("Final this.field parsing")
			@Tag("sarlValidation")
			public void parsing_finalThisField() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final this.field compiling")
			public void compiling_finalThisField() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String IMPLICIT_THIS_FINAL_FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  val field : int = 5",
					"  def fct(param : int = field) { }",
					"}");

			@Test
			@DisplayName("Final field parsing without this")
			@Tag("sarlValidation")
			public void parsing_finalFieldDirect() throws Exception {
				SarlScript mas = file(getParseHelper(), IMPLICIT_THIS_FINAL_FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Final field compiling without this")
			public void compiling_finalFieldDirect() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", IMPLICIT_THIS_FINAL_FIELD, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private final int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return this.field;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  var field : int = 5",
					"  def fct(param : int = field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling")
			public void compiling_fieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String THIS_EXPRESSION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  var field : int = 5",
					"  def fct(param : int = this.field * 7) { }",
					"}");

			@Test
			@DisplayName("Expression parsing with this")
			@Tag("sarlValidation")
			public void parsing_thisFieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), THIS_EXPRESSION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Expression compiling with this")
			public void compiling_thisFieldExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", THIS_EXPRESSION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  private int field = 5;",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"this.field * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    return (this.field * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public boolean equals(final Object obj) {",
						"    if (this == obj)",
						"      return true;",
						"    if (obj == null)",
						"      return false;",
						"    if (getClass() != obj.getClass())",
						"      return false;",
						"    XXX other = (XXX) obj;",
						"    if (other.field != this.field)",
						"      return false;",
						"    return super.equals(obj);",
						"  }",
						"  ",
						"  @Override",
						"  @Pure",
						"  @SyntheticMember",
						"  public int hashCode() {",
						"    int result = super.hashCode();",
						"    final int prime = 31;",
						"    result = prime * result + Integer.hashCode(this.field);",
						"    return result;",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

		}

		/** Referencing an instance function.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from function")
		@Nested
		public class FunctionReference extends AbstractSarlTest {

			private final String PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX) { }",
					"}");

			@Test
			@DisplayName("Pure function parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function compiling")
			public void compiling_pureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @Pure",
						"  public int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return _x;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  def setX : int {5}",
					"  def fct(param : int = setX) { }",
					"}");

			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX");
			}

			private final String PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  def getX : int {5}",
					"  def fct(param : int = getX * 7) { }",
					"}");

			@Test
			@DisplayName("Pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_pureFunctionExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Pure function expression compiling")
			public void compiling_pureFunctionExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", PURE_FUNCTION_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Agent;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @Pure",
						"  public int getX() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueSource",
						"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#FCT_0\") final int param) {",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"getX * 7\")",
						"  private final int $DEFAULT_VALUE$FCT_0() {",
						"    int _x = this.getX();",
						"    return (_x * 7);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0());",
						"  }",
						"  ",
						"  @SyntheticMember",
						"  public XXX(final Agent arg0) {",
						"    super(arg0);",
						"  }",
						"}",
						""));
			}

			private final String NOT_PURE_FUNCTION_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  var field : int",
					"  def getX : int {return this.field}",
					"  def setX(a : int) : int {this.field = a; return a}",
					"  def fct(param : int = setX(4) * 7) { }",
					"}");

			@Test
			@DisplayName("Not pure function expression parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunctionExr() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference the not-pure operation: setX(int) : int");
			}

		}

		/** Referencing a constant from a constructor.
		 *
		 * @author $Author: sgalland$
		 * @version $Name$ $Revision$ $Date$
		 * @mavengroupid $GroupId$
		 * @mavenartifactid $ArtifactId$
		 */
		@DisplayName("Function reference from constructor")
		@Nested
		public class ConstantReferenceConstructor extends AbstractSarlTest {

			private final String CONSTANT = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  new (param : int = 5) { super(null) }",
					"}");


			@Test
			@DisplayName("Constant parsing")
			@Tag("sarlValidation")
			public void parsing_constant() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant compiling")
			public void compiling_constant() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return 5;",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String CONSTANT_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  new (param : int = 5 * 6 + 5) { super(null) }",
					"}");

			@Test
			@DisplayName("Constant expression parsing")
			@Tag("sarlValidation")
			public void parsing_constantExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), CONSTANT_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}

			@Test
			@Tag("compileToJava")
			@DisplayName("Constant expression compiling")
			public void compiling_constantExpr() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", CONSTANT_EXPR, multilineString(
						"package io.sarl.lang.tests.bug612;",
						"",
						"import io.sarl.lang.annotation.DefaultValue;",
						"import io.sarl.lang.annotation.DefaultValueSource;",
						"import io.sarl.lang.annotation.DefaultValueUse;",
						"import io.sarl.lang.annotation.SarlElementType;",
						"import io.sarl.lang.annotation.SarlSourceCode;",
						"import io.sarl.lang.annotation.SarlSpecification;",
						"import io.sarl.lang.annotation.SyntheticMember;",
						"import io.sarl.lang.core.Behavior;",
						"import org.eclipse.xtext.xbase.lib.Pure;",
						"",
						"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
						"@SarlElementType(" + SarlPackage.SARL_BEHAVIOR + ")",
						"@SuppressWarnings(\"all\")",
						"public class XXX extends Behavior {",
						"  @DefaultValueSource",
						"  public XXX(@DefaultValue(\"io.sarl.lang.tests.bug612.XXX#NEW_0\") final int param) {",
						"    super(null);",
						"  }",
						"  ",
						"  /**",
						"   * Default value for the parameter param",
						"   */",
						"  @Pure",
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static int $DEFAULT_VALUE$NEW_0() {",
						"    return ((5 * 6) + 5);",
						"  }",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0());",
						"  }",
						"}",
						""));
			}

			private final String FIELD = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  val field : int = 5",
					"  new (param : int = field) { super(null) }",
					"}");

			@Test
			@DisplayName("Instance field parsing")
			@Tag("sarlValidation")
			public void parsing_field() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

			private final String FIELD_EXPR = multilineString(
					"package io.sarl.lang.tests.bug612",
					"behavior XXX {",
					"  val field : int = 5",
					"  new (param : int = field * 7) { super(null) }",
					"}");

			@Test
			@DisplayName("Instance field expression parsing")
			@Tag("sarlValidation")
			public void parsing_fieldExpr() throws Exception {
				SarlScript mas = file(getParseHelper(), FIELD_EXPR);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertError(
						XbasePackage.eINSTANCE.getXFeatureCall(),
						STATIC_ACCESS_TO_INSTANCE_MEMBER,
						"static reference to the non-static field field");
			}

		}

	}

	/** Test specific skill issues
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Skill Issues")
	@Nested
	public class SkillIssues extends AbstractSarlTest {

		private final String SOURCE_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import foo.Csts",
				"import java.util.Collection",
				"import java.util.List",
				"import java.util.UUID",
				"import io.sarl.core.Initialize",
				"import org.eclipse.xtend.lib.annotations.Accessors",
				"capacity TestingCapacity {",
				"	def getSpawner : UUID",
				"	def addResult(result : Object)",
				"	@Pure",
				"	def getNumberOfResults : int",
				"	def addResults(results : Collection<?>)",
				"	@Pure",
				"	def getUnmodifiableResults : List<Object>",
				"	@Pure",
				"	def getAgentInitializationParameters : Object[]",
				"	@Pure",
				"	def getParam(index : int) : T with T",
				"	@Pure",
				"	def getParam(type : Class<T>, index : int) : T with T",
				"	@Pure",
				"	def buildAgentInitializationParameters(values : Object*) : Object[]",
				"	def waitAndDo(condition : () => boolean, timeout : int = Csts::VALUE, code : ()=>void)",
				"	def killMeSoon",
				"}",
				"skill TestingSkill implements TestingCapacity {",
				"	@Accessors(PUBLIC_GETTER)",
				"	var spawner : UUID",
				"	new (^event : Initialize) {",
				"	}",
				"	override uninstall {",
				"	}",
				"	override addResult(result : Object) {",
				"	}",
				"	@Pure",
				"	override getNumberOfResults : int {",
				"     return 0",
				"	}",
				"	override addResults(results : Collection<?>) {",
				"	}",
				"	override getUnmodifiableResults : List<Object> {",
				"     return null",
				"	}",
				"	@Pure",
				"	def getModifiableResults : List<Object> {",
				"     return null",
				"	}",
				"	@Pure",
				"	override getAgentInitializationParameters : Object[] {",
				"     return null",
				"	}",
				"	@Pure",
				"	override buildAgentInitializationParameters(values : Object*) : Object[] {",
				"     return null",
				"	}",
				"	@Pure",
				"	override getParam(index : int) : T with T {",
				"     return null",
				"	}",
				"	@Pure",
				"	override getParam(type : Class<T>, index : int) : T with T {",
				"     return null",
				"	}",
				"	override waitAndDo(condition : ()=>boolean, timeout : int, code : ()=>void) {",
				"	}",
				"	override killMeSoon {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing")
		@Tag("sarlValidation")
		public void parsing0() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Skill Compiling")
		public void compilingSkill0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.TestingSkill", SOURCE_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.core.Initialize;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Skill;",
					"import java.util.Collection;",
					"import java.util.List;",
					"import java.util.Objects;",
					"import java.util.UUID;",
					"import org.eclipse.xtend.lib.annotations.AccessorType;",
					"import org.eclipse.xtend.lib.annotations.Accessors;",
					"import org.eclipse.xtext.xbase.lib.Functions.Function0;",
					"import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class TestingSkill extends Skill implements TestingCapacity {",
					"  @Accessors(AccessorType.PUBLIC_GETTER)",
					"  private UUID spawner;",
					"  ",
					"  public TestingSkill(final Initialize event) {",
					"  }",
					"  ",
					"  @Override",
					"  public void uninstall() {",
					"  }",
					"  ",
					"  @Override",
					"  public void addResult(final Object result) {",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public int getNumberOfResults() {",
					"    return 0;",
					"  }",
					"  ",
					"  @Override",
					"  public void addResults(final Collection<?> results) {",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  public List<Object> getUnmodifiableResults() {",
					"    return null;",
					"  }",
					"  ",
					"  @Pure",
					"  public List<Object> getModifiableResults() {",
					"    return null;",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public Object[] getAgentInitializationParameters() {",
					"    return null;",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public Object[] buildAgentInitializationParameters(final Object... values) {",
					"    return null;",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public <T extends Object> T getParam(final int index) {",
					"    return null;",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public <T extends Object> T getParam(final Class<T> type, final int index) {",
					"    return null;",
					"  }",
					"  ",
					"  @Override",
					"  @DefaultValueSource",
					"  public void waitAndDo(final Function0<? extends Boolean> condition, @DefaultValue(\"io.sarl.lang.tests.bug612.TestingCapacity#WAITANDDO_0\") final int timeout, final Procedure0 code) {",
					"  }",
					"  ",
					"  @Override",
					"  public void killMeSoon() {",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public boolean equals(final Object obj) {",
					"    if (this == obj)",
					"      return true;",
					"    if (obj == null)",
					"      return false;",
					"    if (getClass() != obj.getClass())",
					"      return false;",
					"    TestingSkill other = (TestingSkill) obj;",
					"    if (!Objects.equals(this.spawner, other.spawner))",
					"      return false;",
					"    return super.equals(obj);",
					"  }",
					"  ",
					"  @Override",
					"  @Pure",
					"  @SyntheticMember",
					"  public int hashCode() {",
					"    int result = super.hashCode();",
					"    final int prime = 31;",
					"    result = prime * result + Objects.hashCode(this.spawner);",
					"    return result;",
					"  }",
					"  ",
					"  @Pure",
					"  @Override",
					"  public UUID getSpawner() {",
					"    return this.spawner;",
					"  }",
					"}",
					""));
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Capacity Compiling")
		public void compilingCapacity0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.TestingCapacity", SOURCE_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Csts;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"import java.util.Collection;",
					"import java.util.List;",
					"import java.util.UUID;",
					"import org.eclipse.xtext.xbase.lib.Functions.Function0;",
					"import org.eclipse.xtext.xbase.lib.Procedures.Procedure0;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface TestingCapacity extends Capacity {",
					"  @Pure",
					"  UUID getSpawner();",
					"  ",
					"  void addResult(final Object result);",
					"  ",
					"  @Pure",
					"  int getNumberOfResults();",
					"  ",
					"  void addResults(final Collection<?> results);",
					"  ",
					"  @Pure",
					"  List<Object> getUnmodifiableResults();",
					"  ",
					"  @Pure",
					"  Object[] getAgentInitializationParameters();",
					"  ",
					"  @Pure",
					"  <T extends Object> T getParam(final int index);",
					"  ",
					"  @Pure",
					"  <T extends Object> T getParam(final Class<T> type, final int index);",
					"  ",
					"  @Pure",
					"  Object[] buildAgentInitializationParameters(final Object... values);",
					"  ",
					"  @DefaultValueSource",
					"  void waitAndDo(final Function0<? extends Boolean> condition, @DefaultValue(\"io.sarl.lang.tests.bug612.TestingCapacity#WAITANDDO_0\") final int timeout, final Procedure0 code);",
					"  ",
					"  void killMeSoon();",
					"  ",
					"  /**",
					"   * Default value for the parameter timeout",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"Csts::VALUE\")",
					"  default int $DEFAULT_VALUE$WAITANDDO_0() {",
					"    return Csts.VALUE;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"()=>boolean,int,()=>void\")",
					"  @SyntheticMember",
					"  default void waitAndDo(final Function0<? extends Boolean> condition, final Procedure0 code) {",
					"    waitAndDo(condition, $DEFAULT_VALUE$WAITANDDO_0(), code);",
					"  }",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends TestingCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements TestingCapacity {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public UUID getSpawner() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getSpawner();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void addResult(final Object result) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.addResult(result);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public int getNumberOfResults() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getNumberOfResults();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void addResults(final Collection<?> results) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.addResults(results);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public List<Object> getUnmodifiableResults() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getUnmodifiableResults();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public Object[] getAgentInitializationParameters() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getAgentInitializationParameters();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public <T extends Object> T getParam(final int index) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getParam(index);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public <T extends Object> T getParam(final Class<T> type, final int index) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.getParam(type, index);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public Object[] buildAgentInitializationParameters(final Object... values) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.buildAgentInitializationParameters(values);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void waitAndDo(final Function0<? extends Boolean> condition, final int timeout, final Procedure0 code) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.waitAndDo(condition, timeout, code);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void killMeSoon() {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.killMeSoon();",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"    ",
					"    public void waitAndDo(final Function0<? extends Boolean> condition, final Procedure0 code) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        this.capacity.waitAndDo(condition, code);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""));
		}

		private final String SOURCE_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"capacity TestingCapacity {",
				"	@Pure",
				"	def buildAgentInitializationParameters(values : Object*) : Object[]",
				"}");

		@Test
		@DisplayName("Parsing 2")
		@Tag("sarlValidation")
		public void parsing1() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Capacity 2 Compiling")
		public void compilingCapacity1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.TestingCapacity", SOURCE_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.core.AgentTrait;",
					"import io.sarl.lang.core.Capacity;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@FunctionalInterface",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CAPACITY + ")",
					"@SuppressWarnings(\"all\")",
					"public interface TestingCapacity extends Capacity {",
					"  @Pure",
					"  Object[] buildAgentInitializationParameters(final Object... values);",
					"  ",
					"  /**",
					"   * @ExcludeFromApidoc",
					"   */",
					"  class ContextAwareCapacityWrapper<C extends TestingCapacity> extends Capacity.ContextAwareCapacityWrapper<C> implements TestingCapacity {",
					"    public ContextAwareCapacityWrapper(final C capacity, final AgentTrait caller) {",
					"      super(capacity, caller);",
					"    }",
					"    ",
					"    public Object[] buildAgentInitializationParameters(final Object... values) {",
					"      try {",
					"        ensureCallerInLocalThread();",
					"        return this.capacity.buildAgentInitializationParameters(values);",
					"      } finally {",
					"        resetCallerInLocalThread();",
					"      }",
					"    }",
					"  }",
					"}",
					""));
		}

	}

	/** Test specific parameter issues
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Parameter Issues")
	@Nested
	public class ParameterIssues extends AbstractSarlTest {

		private final String SARL_INTERFACE_IMPLEMENTATION_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"interface Type1 {",
				"	def fct0(param0 : String = \"abc\", param1 : int)",
				"}",
				"class Type2 implements Type1 {",
				"	def fct0(param0 : String = \"abc\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing SARL interface implementation with same value")
		@Tag("sarlValidation")
		public void parsingSarlInterfaceImplementation0() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_INTERFACE_IMPLEMENTATION_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter param0")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling SARL interface implementation with same value")
		public void compilingSarlInterfaceImplementation0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type2", SARL_INTERFACE_IMPLEMENTATION_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type2 implements Type1 {",
					"  @DefaultValueSource",
					"  public void fct0(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT0_0\") final String param0, final int param1) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type2() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SARL_INTERFACE_IMPLEMENTATION_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type2 implements foo.Type1 {",
				"	def fct0(param0 : String = \"abc\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing JVM interface implementation with same value")
		@Tag("sarlValidation")
		public void parsingSarlInterfaceImplementation1() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_INTERFACE_IMPLEMENTATION_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter param0")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling JVM interface implementation with same value")
		public void compilingSarlInterfaceImplementation1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type2", SARL_INTERFACE_IMPLEMENTATION_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Type1;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type2 implements Type1 {",
					"  @DefaultValueSource",
					"  public void fct0(@DefaultValue(\"foo.Type1#FCT0_0\") final String param0, final int param1) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type2() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SARL_CLASS_EXTENSION_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"abstract class Type1 {",
				"	abstract def fct0(param0 : String = \"abc\", param1 : int)",
				"}",
				"class Type2 extends Type1 {",
				"	def fct0(param0 : String = \"abc\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing SARL class extension with same value")
		@Tag("sarlValidation")
		public void parsingSarlClassExtension0() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CLASS_EXTENSION_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter param0")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling SARL class extension with same value")
		public void compilingSarlClassExtension0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type2", SARL_CLASS_EXTENSION_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type2 extends Type1 {",
					"  @DefaultValueSource",
					"  public void fct0(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT0_0\") final String param0, final int param1) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type2() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SARL_CLASS_EXTENSION_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type3 extends foo.Type2 {",
				"	def fct0(param0 : String = \"abc\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing JVM class extension with same value")
		@Tag("sarlValidation")
		public void parsingSarlClassExtension1() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CLASS_EXTENSION_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter param0")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling JVM class extension with same value")
		public void compilingSarlClassExtension1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type3", SARL_CLASS_EXTENSION_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Type2;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type3 extends Type2 {",
					"  @DefaultValueSource",
					"  public void fct0(@DefaultValue(\"foo.Type2#FCT0_0\") final String param0, final int param1) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type3() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SARL_INTERFACE_IMPLEMENTATION_2 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"interface Type1 {",
				"	def fct0(param0 : String = \"abc\", param1 : int)",
				"}",
				"class Type2 implements Type1 {",
				"	def fct0(param0 : String = \"abcd\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing SARL interface implementation with different values")
		@Tag("sarlValidation")
		public void parsingSarlInterfaceImplementation2() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_INTERFACE_IMPLEMENTATION_2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION,
					"Illegal redefinition of the default value for the formal parameter param0",
					"Inherited value is: \"abc\"",
					"While the current value is: \"abcd\"");
		}

		private final String SARL_INTERFACE_IMPLEMENTATION_3 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type2 implements foo.Type1 {",
				"	def fct0(param0 : String = \"abcd\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing JVM interface implementation with different values")
		@Tag("sarlValidation")
		public void parsingSarlInterfaceImplementation3() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_INTERFACE_IMPLEMENTATION_3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION,
					"Illegal redefinition of the default value for the formal parameter param0",
					"Inherited value is: \"abc\"",
					"While the current value is: \"abcd\"");
		}

		private final String SARL_CLASS_EXTENSION_2 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"abstract class Type1 {",
				"	abstract def fct0(param0 : String = \"abc\", param1 : int)",
				"}",
				"class Type2 extends Type1 {",
				"	def fct0(param0 : String = \"abcd\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing SARL class extension with different values")
		@Tag("sarlValidation")
		public void parsingSarlClassExtension2() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CLASS_EXTENSION_2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION,
					"Illegal redefinition of the default value for the formal parameter param0",
					"Inherited value is: \"abc\"",
					"While the current value is: \"abcd\"");
		}

		private final String SARL_CLASS_EXTENSION_3 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type2 extends foo.Type2 {",
				"	def fct0(param0 : String = \"abcd\", param1 : int) {}",
				"}");

		@Test
		@DisplayName("Parsing JVM class extension with different values")
		@Tag("sarlValidation")
		public void parsingSarlClassExtension3() throws Exception {
			SarlScript mas = file(getParseHelper(), SARL_CLASS_EXTENSION_3);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertError(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					ILLEGAL_PARAMETER_DEFAULT_VALUE_REDEFINITION,
					"Illegal redefinition of the default value for the formal parameter param0",
					"Inherited value is: \"abc\"",
					"While the current value is: \"abcd\"");
		}

	}


	/** Test specific to capacity implementation issues
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Capacity Implementation Issues")
	@Nested
	public class CapacityImplementationIssues extends AbstractSarlTest {

		private final String SOURCE_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import io.sarl.lang.core.Agent",
				"import io.sarl.core.AgentTask",
				"capacity MyCapacity {",
				"  def execute(task : AgentTask = null, procedure : (Agent) => void) : AgentTask",
				"}",
				"skill MySkill implements MyCapacity {",
				"  def execute(task : AgentTask = null, procedure : (Agent) => void) : AgentTask {",
				"    null",
				"  }",
				"}");

		@Test
		@DisplayName("Parsing single script")
		@Tag("sarlValidation")
		public void parsing0() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter task")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling single script")
		public void compiling0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.MySkill", SOURCE_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.core.AgentTask;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class MySkill extends Skill implements MyCapacity {",
					"  @DefaultValueSource",
					"  public AgentTask execute(@DefaultValue(\"io.sarl.lang.tests.bug612.MyCapacity#EXECUTE_0\") final AgentTask task, final Procedure1<? super Agent> procedure) {",
					"    return null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public MySkill() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public MySkill(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""));
		}

		private final String SOURCE_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import io.sarl.lang.core.Agent",
				"import foo.MockCapacity2",
				"skill MySkill implements MockCapacity2 {",
				"  def execute(task : Object = null, procedure : (Agent) => void) : Object {",
				"    null",
				"  }",
				"}");

		@Test
		@DisplayName("Parsing script with JVM elements")
		@Tag("sarlValidation")
		public void parsing1() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter task")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling script with JVM elements")
		public void compiling1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.MySkill", SOURCE_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.MockCapacity2;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.Agent;",
					"import io.sarl.lang.core.Skill;",
					"import org.eclipse.xtext.xbase.lib.Procedures.Procedure1;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_SKILL + ")",
					"@SuppressWarnings(\"all\")",
					"public class MySkill extends Skill implements MockCapacity2 {",
					"  @DefaultValueSource",
					"  public Object execute(@DefaultValue(\"foo.MockCapacity2#EXECUTE_0\") final Object task, final Procedure1<? super Agent> procedure) {",
					"    return null;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public MySkill() {",
					"    super();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public MySkill(final Agent arg0) {",
					"    super(arg0);",
					"  }",
					"}",
					""));
		}

	}

	/** Test specific to implementation issues
	 *
	 * @author $Author: sgalland$
	 * @version $Name$ $Revision$ $Date$
	 * @mavengroupid $GroupId$
	 * @mavenartifactid $ArtifactId$
	 */
	@DisplayName("Implementation Issues")
	@Nested
	public class ImplementationIssues extends AbstractSarlTest {

		private final String SOURCE_OVERRIDE_NOT_DEFAULT_VALUED_ACTIONS = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import io.sarl.lang.core.AgentContext",
				"interface SREBootstrap {",
				"   def startWithoutAgent(asCommandLineApp : boolean) : AgentContext",
				"	def startWithoutAgent : AgentContext {",
				"		return startWithoutAgent(false)",
				"	}",
				"}",
				"class SreMain implements SREBootstrap {",
				"	def startWithoutAgent(asCommandLineApp : boolean, bootique : Object = null, module : Class<?> = null) : AgentContext {",
				"		null",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing override not-default-valued param actions")
		@Tag("sarlValidation")
		public void parsingOverrideNotDefaultValuedParamActions() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_OVERRIDE_NOT_DEFAULT_VALUED_ACTIONS);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling override not-default-valued param actions")
		public void compilingOverrideNotDefaultValuedParamActions() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.SreMain", SOURCE_OVERRIDE_NOT_DEFAULT_VALUED_ACTIONS, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import io.sarl.lang.core.AgentContext;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class SreMain implements SREBootstrap {",
					"  @DefaultValueSource",
					"  @Pure",
					"  public AgentContext startWithoutAgent(final boolean asCommandLineApp, @DefaultValue(\"io.sarl.lang.tests.bug612.SreMain#STARTWITHOUTAGENT_0\") final Object bootique, @DefaultValue(\"io.sarl.lang.tests.bug612.SreMain#STARTWITHOUTAGENT_1\") final Class<?> module) {",
					"    return null;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter bootique",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private final Object $DEFAULT_VALUE$STARTWITHOUTAGENT_0() {",
					"    return null;",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter module",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"null\")",
					"  private final Class $DEFAULT_VALUE$STARTWITHOUTAGENT_1() {",
					"    return null;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"boolean,java.lang.Object,java.lang.Class\")",
					"  @SyntheticMember",
					"  @Pure",
					"  public final AgentContext startWithoutAgent(final boolean asCommandLineApp) {",
					"    return startWithoutAgent(asCommandLineApp, $DEFAULT_VALUE$STARTWITHOUTAGENT_0(), $DEFAULT_VALUE$STARTWITHOUTAGENT_1());",
					"  }",
					"  ",
					"  @DefaultValueUse(\"boolean,java.lang.Object,java.lang.Class\")",
					"  @SyntheticMember",
					"  @Pure",
					"  public final AgentContext startWithoutAgent(final boolean asCommandLineApp, final Class<?> module) {",
					"    return startWithoutAgent(asCommandLineApp, $DEFAULT_VALUE$STARTWITHOUTAGENT_0(), module);",
					"  }",
					"  ",
					"  @DefaultValueUse(\"boolean,java.lang.Object,java.lang.Class\")",
					"  @SyntheticMember",
					"  @Pure",
					"  public final AgentContext startWithoutAgent(final boolean asCommandLineApp, final Object bootique) {",
					"    return startWithoutAgent(asCommandLineApp, bootique, $DEFAULT_VALUE$STARTWITHOUTAGENT_1());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public SreMain() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SOURCE_TYPE_NOT_FOUND_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import java.util.UUID",
				"interface SreRunContext {",
				"	def indexOfResult(agentId : UUID = null, type : Class<?>, fromIndex : int = 5) : int",
				"}",
				"class SreRunContextImpl implements SreRunContext {",
				"	override indexOfResult(agentId : UUID = null, type : Class<?>, fromIndex : int = 5) : int {",
				"		return 0",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing for type not found error")
		@Tag("sarlValidation")
		public void parsingTypeNotFound0() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_TYPE_NOT_FOUND_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter agentId")
				.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter fromIndex")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling for type not found error")
		public void compilingTypeNotFound0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.SreRunContextImpl", SOURCE_TYPE_NOT_FOUND_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.UUID;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class SreRunContextImpl implements SreRunContext {",
					"  @DefaultValueSource",
					"  @Override",
					"  public int indexOfResult(@DefaultValue(\"io.sarl.lang.tests.bug612.SreRunContext#INDEXOFRESULT_0\") final UUID agentId, final Class<?> type, @DefaultValue(\"io.sarl.lang.tests.bug612.SreRunContext#INDEXOFRESULT_1\") final int fromIndex) {",
					"    return 0;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public SreRunContextImpl() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String SOURCE_TYPE_NOT_FOUND_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import java.util.UUID",
				"interface SreRunContext {",
				"	def indexOfResult(agentId : UUID = null, type : Class<?>, fromIndex : int = 5) : int",
				"}",
				"class SreRunContextImpl implements SreRunContext {",
				"	override indexOfResult(agentId : UUID, type : Class<?>, fromIndex : int) : int {",
				"		return 0",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing for type not found error - 2")
		@Tag("sarlValidation")
		public void parsingTypeNotFound1() throws Exception {
			SarlScript mas = file(getParseHelper(), SOURCE_TYPE_NOT_FOUND_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling for type not found error - 2")
		public void compilingTypeNotFound1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.SreRunContextImpl", SOURCE_TYPE_NOT_FOUND_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import java.util.UUID;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class SreRunContextImpl implements SreRunContext {",
					"  @Override",
					"  @DefaultValueSource",
					"  public int indexOfResult(@DefaultValue(\"io.sarl.lang.tests.bug612.SreRunContext#INDEXOFRESULT_0\") final UUID agentId, final Class<?> type, @DefaultValue(\"io.sarl.lang.tests.bug612.SreRunContext#INDEXOFRESULT_1\") final int fromIndex) {",
					"    return 0;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public SreRunContextImpl() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_STATIC_FIELD_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type0 {",
				"	public static var CST : int = 123",
				"}",
				"class Type1 {",
				"	def fct(param : int = Type0::CST) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to static field")
		@Tag("sarlValidation")
		public void parsingReferenceToStaticField0() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_STATIC_FIELD_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to static field")
		public void compilingReferenceToStaticField0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_STATIC_FIELD_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 {",
					"  @DefaultValueSource",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter param",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"Type0::CST\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return Type0.CST;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public final void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_STATIC_FIELD_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import foo.Csts",
				"class Type1 {",
				"	def fct(param : int = Csts::VALUE) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to external static field")
		@Tag("sarlValidation")
		public void parsingReferenceToStaticField1() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_STATIC_FIELD_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to external static field")
		public void compilingReferenceToStaticField1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_STATIC_FIELD_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Csts;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 {",
					"  @DefaultValueSource",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter param",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"Csts::VALUE\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return Csts.VALUE;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public final void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_STATIC_FINAL_FIELD_0 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"class Type0 {",
				"	public static val CST : int = 123",
				"}",
				"class Type1 {",
				"	def fct(param : int = Type0::CST) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to static field")
		@Tag("sarlValidation")
		public void parsingReferenceToStaticFinalField0() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_STATIC_FINAL_FIELD_0);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to static field")
		public void compilingReferenceToStaticFinalField0() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_STATIC_FINAL_FIELD_0, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 {",
					"  @DefaultValueSource",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter param",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"Type0::CST\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return Type0.CST;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public final void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_STATIC_FINAL_FIELD_1 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import foo.Csts",
				"class Type1 {",
				"	def fct(param : int = Csts::FINAL_VALUE) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to external static final field")
		@Tag("sarlValidation")
		public void parsingReferenceToStaticFinalField1() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_STATIC_FINAL_FIELD_1);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to external static final field")
		public void compilingReferenceToStaticFinalField1() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_STATIC_FINAL_FIELD_1, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Csts;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 {",
					"  @DefaultValueSource",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter param",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"Csts::FINAL_VALUE\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return Csts.FINAL_VALUE;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public final void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_STATIC_FINAL_FIELD_2 = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import static extension foo.Csts.*",
				"class Type1 {",
				"	def fct(param : int = FINAL_VALUE) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to external static final field 2")
		@Tag("sarlValidation")
		public void parsingReferenceToStaticFinalField2() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_STATIC_FINAL_FIELD_2);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to external static final field 2")
		public void compilingReferenceToStaticFinalField2() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_STATIC_FINAL_FIELD_2, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import foo.Csts;",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.DefaultValueUse;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSourceCode;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 {",
					"  @DefaultValueSource",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type1#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  /**",
					"   * Default value for the parameter param",
					"   */",
					"  @Pure",
					"  @SyntheticMember",
					"  @SarlSourceCode(\"FINAL_VALUE\")",
					"  private final int $DEFAULT_VALUE$FCT_0() {",
					"    return Csts.FINAL_VALUE;",
					"  }",
					"  ",
					"  @DefaultValueUse(\"int\")",
					"  @SyntheticMember",
					"  public final void fct() {",
					"    fct($DEFAULT_VALUE$FCT_0());",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

		private final String REFERENCE_TO_INHERITED_STATIC_FINAL_FIELD = multilineString(
				"package io.sarl.lang.tests.bug612",
				"import static extension foo.Csts.*",
				"interface Type0 {",
				"	def fct(param : int = FINAL_VALUE)",
				"}",
				"class Type1 implements Type0 {",
				"	override fct(param : int = FINAL_VALUE) : void {",
				"	}",
				"}");

		@Test
		@DisplayName("Parsing reference to inherited external static final field")
		@Tag("sarlValidation")
		public void parsingReferenceToInheritedStaticFinalField() throws Exception {
			SarlScript mas = file(getParseHelper(), REFERENCE_TO_INHERITED_STATIC_FINAL_FIELD);
			final Validator validator = validate(getValidationHelper(), getInjector(), mas);
			validator.assertWarning(
					SarlPackage.Literals.SARL_FORMAL_PARAMETER,
					PARAMETER_DEFAULT_VALUE_REDFINITION,
					"Redundant definition of the default value for the formal parameter param")
				.assertNoIssues();
		}

		@Test
		@Tag("compileToJava")
		@DisplayName("Compiling reference to inherited external static final field")
		public void compilingReferenceToInheritedStaticFinalField() throws Exception {
			getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.Type1", REFERENCE_TO_INHERITED_STATIC_FINAL_FIELD, multilineString(
					"package io.sarl.lang.tests.bug612;",
					"",
					"import io.sarl.lang.annotation.DefaultValue;",
					"import io.sarl.lang.annotation.DefaultValueSource;",
					"import io.sarl.lang.annotation.SarlElementType;",
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
					"@SuppressWarnings(\"all\")",
					"public class Type1 implements Type0 {",
					"  @DefaultValueSource",
					"  @Override",
					"  public void fct(@DefaultValue(\"io.sarl.lang.tests.bug612.Type0#FCT_0\") final int param) {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public Type1() {",
					"    super();",
					"  }",
					"}",
					""));
		}

	}

}