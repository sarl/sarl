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

	/** Referencing a constant. 
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static final int $DEFAULT_VALUE$FCT_0 = 5;",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static final int $DEFAULT_VALUE$FCT_0 = ((5 * 6) + 5);",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public final void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0);",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
					"  static def pureFunction : int {field = 5; return 5}",
					"  def fct(param : int = pureFunction) { }",
					"}");
	
			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}
	
			@Test
			@Tag("compileToJava")
			@DisplayName("Not pure function compiling")
			public void compiling_notPureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", NOT_PURE_FUNCTION, multilineString(
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
						"  private static int field;",
						"  ",
						"  @Pure",
						"  public static int getX() {",
						"    return XXX.field;",
						"  }",
						"  ",
						"  public static int pureFunction() {",
						"    XXX.field = 5;",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
					"  def fct(param : int = getX) { }",
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
						"Default value's expression cannot reference to a not-pure operation");
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
						XbasePackage.eINSTANCE.getXBinaryOperation(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference to a not-pure operation");
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static final int $DEFAULT_VALUE$FCT_0 = 5;",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static final int $DEFAULT_VALUE$FCT_0 = ((5 * 6) + 5);",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public static void fct() {",
						"    fct($DEFAULT_VALUE$FCT_0);",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
					"  static def pureFunction : int {field = 5; return 5}",
					"  static def fct(param : int = pureFunction) { }",
					"}");
	
			@Test
			@DisplayName("Not pure function parsing")
			@Tag("sarlValidation")
			public void parsing_notPureFunction() throws Exception {
				SarlScript mas = file(getParseHelper(), NOT_PURE_FUNCTION);
				final Validator validator = validate(getValidationHelper(), getInjector(), mas);
				validator.assertNoIssues();
			}
	
			@Test
			@Tag("compileToJava")
			@DisplayName("Not pure function compiling")
			public void compiling_notPureFunction() throws Exception {
				getCompileHelper().assertCompilesTo("io.sarl.lang.tests.bug612.XXX", NOT_PURE_FUNCTION, multilineString(
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
						"  private static int field;",
						"  ",
						"  @Pure",
						"  public static int getX() {",
						"    return XXX.field;",
						"  }",
						"  ",
						"  public static int pureFunction() {",
						"    XXX.field = 5;",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
						"  private static final int $DEFAULT_VALUE$FCT_0() {",
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
					"  static def fct(param : int = getX) { }",
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
						"Default value's expression cannot reference to a not-pure operation");
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
						XbasePackage.eINSTANCE.getXBinaryOperation(),
						FORBIDDEN_REFERENCE,
						"Default value's expression cannot reference to a not-pure operation");
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = 5;",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"5 * 6 + 5\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = ((5 * 6) + 5);",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = XXX.SVAL;",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"SVAL * 7\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = (XXX.SVAL * 7);",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = XXX.sval();",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
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
						"  @SyntheticMember",
						"  @SarlSourceCode(\"sval * 7\")",
						"  private static final int $DEFAULT_VALUE$NEW_0 = (XXX.sval() * 7);",
						"  ",
						"  @DefaultValueUse(\"int\")",
						"  @SyntheticMember",
						"  public XXX() {",
						"    this($DEFAULT_VALUE$NEW_0);",
						"  }",
						"}",
						""));
			}

		}

	}

}
