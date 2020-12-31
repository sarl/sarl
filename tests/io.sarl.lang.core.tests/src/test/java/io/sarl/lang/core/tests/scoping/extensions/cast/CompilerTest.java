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

package io.sarl.lang.core.tests.scoping.extensions.cast;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@GlobalCompilationSuite
@SuppressWarnings("all")
@DisplayName("Cast operators - compilation")
@Tag("core")
public class CompilerTest extends AbstractSarlTest {

	private static final String STRING_AS_BOOLEAN_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : boolean {",
			"    left as boolean",
			"  }",
			"}");

	private static final String STRING_AS_BOOLEAN_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public boolean fct(final String left) {",
			"    return (left == null ? false : Boolean.parseBoolean((left).toString()));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_boolean_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_BOOLEAN_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'booleanValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_boolean(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_BOOLEAN_SARL, STRING_AS_BOOLEAN_JAVA);
	}

	private static final String BOOLEAN_AS_STRING_SARL = multilineString(
			"class A {",
			"  def fct(left : boolean) : String {",
			"    left as String",
			"  }",
			"}");

	private static final String BOOLEAN_AS_STRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public String fct(final boolean left) {",
			"    return Boolean.toString(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void boolean_as_string_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), BOOLEAN_AS_STRING_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toString'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void boolean_as_string(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(BOOLEAN_AS_STRING_SARL, BOOLEAN_AS_STRING_JAVA);
	}

	private static final String CHAR_AS_STRING_SARL = multilineString(
			"class A {",
			"  def fct(left : char) : String {",
			"    left as String",
			"  }",
			"}");

	private static final String CHAR_AS_STRING_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public String fct(final char left) {",
			"    return Character.toString(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void char_as_string_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), CHAR_AS_STRING_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toString'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void char_as_string(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(CHAR_AS_STRING_SARL, CHAR_AS_STRING_JAVA);
	}

	private static final String STRING_AS_BYTE_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : byte {",
			"    left as byte",
			"  }",
			"}");

	private static final String STRING_AS_BYTE_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public byte fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.byteValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_byte_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_BYTE_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'byteValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_BYTE_SARL, STRING_AS_BYTE_JAVA);
	}

	private static final String STRING_AS_BYTEOBJ_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Byte {",
			"    left as Byte",
			"  }",
			"}");

	private static final String STRING_AS_BYTEOBJ_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Byte fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toByte(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_Byte_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_BYTEOBJ_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'byteValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_BYTEOBJ_SARL, STRING_AS_BYTEOBJ_JAVA);
	}

	private static final String STRING_AS_SHORT_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : short {",
			"    left as short",
			"  }",
			"}");

	private static final String STRING_AS_SHORT_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public short fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.shortValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_short_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_SHORT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'shortValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_SHORT_SARL, STRING_AS_SHORT_JAVA);
	}

	private static final String STRING_AS_SHORTOBJ_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Short {",
			"    left as Short",
			"  }",
			"}");

	private static final String STRING_AS_SHORTOBJ_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Short fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toShort(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_Short_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_SHORTOBJ_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toShort'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_SHORTOBJ_SARL, STRING_AS_SHORTOBJ_JAVA);
	}

	private static final String STRING_AS_INT_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : int {",
			"    left as int",
			"  }",
			"}");

	private static final String STRING_AS_INT_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public int fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.intValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_int_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_INT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'intValue'");
	}


	private static final String STRING_AS_INTEGER_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Integer {",
			"    left as Integer",
			"  }",
			"}");

	private static final String STRING_AS_INTEGER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Integer fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toInteger(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_integer_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_INTEGER_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toInteger'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_INTEGER_SARL, STRING_AS_INTEGER_JAVA);
	}

	private static final String STRING_AS_LONG_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : long {",
			"    left as long",
			"  }",
			"}");

	private static final String STRING_AS_LONG_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public long fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.longValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_long_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_LONG_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'longValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_LONG_SARL, STRING_AS_LONG_JAVA);
	}


	private static final String STRING_AS_LONGOBJ_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Long {",
			"    left as Long",
			"  }",
			"}");

	private static final String STRING_AS_LONGOBJ_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Long fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toLong(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_Long_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_LONGOBJ_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toLong'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_LONGOBJ_SARL, STRING_AS_LONGOBJ_JAVA);
	}

	private static final String STRING_AS_FLOAT_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : float {",
			"    left as float",
			"  }",
			"}");

	private static final String STRING_AS_FLOAT_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public float fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.floatValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_float_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_FLOAT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'floatValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_FLOAT_SARL, STRING_AS_FLOAT_JAVA);
	}

	private static final String STRING_AS_FLOATOBJ_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Float {",
			"    left as Float",
			"  }",
			"}");

	private static final String STRING_AS_FLOATOBJ_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Float fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toFloat(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_Float_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_FLOATOBJ_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toFloat'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_FLOATOBJ_SARL, STRING_AS_FLOATOBJ_JAVA);
	}

	private static final String STRING_AS_DOUBLE_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : double {",
			"    left as double",
			"  }",
			"}");

	private static final String STRING_AS_DOUBLE_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public double fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.doubleValue(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_double_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_DOUBLE_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'doubleValue'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_DOUBLE_SARL, STRING_AS_DOUBLE_JAVA);
	}

	private static final String STRING_AS_DOUBLEOBJ_SARL = multilineString(
			"class A {",
			"  def fct(left : String) : Double {",
			"    left as Double",
			"  }",
			"}");

	private static final String STRING_AS_DOUBLEOBJ_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Double fct(final String left) {",
			"    return (left == null ? 0 : PrimitiveCastExtensions.toDouble(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_Double_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_DOUBLEOBJ_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toDouble'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_DOUBLEOBJ_SARL, STRING_AS_DOUBLEOBJ_JAVA);
	}

	private static final String STRING_AS_ATOMICINTEGER_SARL = multilineString(
			"import java.util.concurrent.atomic.AtomicInteger",
			"class A {",
			"  def fct(left : String) : AtomicInteger {",
			"    left as AtomicInteger",
			"  }",
			"}");

	private static final String STRING_AS_ATOMICINTEGER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.util.concurrent.atomic.AtomicInteger;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicInteger fct(final String left) {",
			"    return (left == null ? null : new AtomicInteger(PrimitiveCastExtensions.intValue(left)));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_atomicinteger_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_ATOMICINTEGER_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toAtomicInteger'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_atomicinteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_ATOMICINTEGER_SARL, STRING_AS_ATOMICINTEGER_JAVA);
	}

	private static final String STRING_AS_ATOMICLONG_SARL = multilineString(
			"import java.util.concurrent.atomic.AtomicLong",
			"class A {",
			"  def fct(left : String) : AtomicLong {",
			"    left as AtomicLong",
			"  }",
			"}");

	private static final String STRING_AS_ATOMICLONG_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.util.concurrent.atomic.AtomicLong;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicLong fct(final String left) {",
			"    return (left == null ? null : new AtomicLong(PrimitiveCastExtensions.longValue(left)));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_atomiclong_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_ATOMICLONG_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toAtomicLong'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_atomiclong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_ATOMICLONG_SARL, STRING_AS_ATOMICLONG_JAVA);
	}

	private static final String STRING_AS_ATOMICDOUBLE_SARL = multilineString(
			"import com.google.common.util.concurrent.AtomicDouble",
			"class A {",
			"  def fct(left : String) : AtomicDouble {",
			"    left as AtomicDouble",
			"  }",
			"}");

	private static final String STRING_AS_ATOMICDOUBLE_JAVA = multilineString(
			"import com.google.common.util.concurrent.AtomicDouble;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicDouble fct(final String left) {",
			"    return (left == null ? null : new AtomicDouble(PrimitiveCastExtensions.doubleValue(left)));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_atomicdouble_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_ATOMICDOUBLE_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toAtomicDouble'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_atomicdouble(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_ATOMICDOUBLE_SARL, STRING_AS_ATOMICDOUBLE_JAVA);
	}

	private static final String STRING_AS_BIGINTEGER_SARL = multilineString(
			"import java.math.BigInteger",
			"class A {",
			"  def fct(left : String) : BigInteger {",
			"    left as BigInteger",
			"  }",
			"}");

	private static final String STRING_AS_BIGINTEGER_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.math.BigInteger;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public BigInteger fct(final String left) {",
			"    return (left == null ? null : PrimitiveCastExtensions.toBigInteger(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_biginteger_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_BIGINTEGER_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toBigInteger'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_biginteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_BIGINTEGER_SARL, STRING_AS_BIGINTEGER_JAVA);
	}

	private static final String STRING_AS_BIGDECIMAL_SARL = multilineString(
			"import java.math.BigDecimal",
			"class A {",
			"  def fct(left : String) : BigDecimal {",
			"    left as BigDecimal",
			"  }",
			"}");

	private static final String STRING_AS_BIGDECIMAL_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.math.BigDecimal;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public BigDecimal fct(final String left) {",
			"    return (left == null ? null : PrimitiveCastExtensions.toBigDecimal(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_bigdecimal_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_BIGDECIMAL_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toBigDecimal'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_bigdecimal(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_BIGDECIMAL_SARL, STRING_AS_BIGDECIMAL_JAVA);
	}

	private static final String STRING_AS_UUID_SARL = multilineString(
			"import java.util.UUID",
			"class A {",
			"  def fct(left : String) : UUID {",
			"    left as UUID",
			"  }",
			"}");

	private static final String STRING_AS_UUID_JAVA = multilineString(
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import io.sarl.lang.scoping.extensions.cast.PrimitiveCastExtensions;",
			"import java.uutil.UUID;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public UUID fct(final String left) {",
			"    return (left == null ? null : PrimitiveCastExtensions.toUUID(left));",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	@Tag("sarlValidation")
	public void string_as_uuid_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), STRING_AS_UUID_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toUUID'");
	}

	@GlobalCompilationTestContribution
	@Tag("compileToJava")
	public void string_as_uuid(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(STRING_AS_UUID_SARL, STRING_AS_UUID_JAVA);
	}

}
