/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2026 SARL.io, the original authors and main authors.
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

package io.sarl.lang.core.tests.scoping.extensions.numbers.cast.longprimitive;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.core.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.api.AbstractSarlTest;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationSuite;
import io.sarl.lang.tests.api.globalcompilation.GlobalCompilationTestContribution;
import io.sarl.lang.tests.api.globalcompilation.ResourceSetGlobalCompilationContext;
import io.sarl.lang.validation.IssueCodes;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-extras/issues/186"
 */
@GlobalCompilationSuite
@SuppressWarnings("all")
@DisplayName("Compiling long cast operator")
@Tag("core")
@Tag("compileToJava")
public class CompilerTest extends AbstractSarlTest {

	private static final String AS_BYTE_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : byte {",
			"    left as byte",
			"  }",
			"}");

	private static final String AS_BYTE_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public byte fct(final long left) {",
			"    return ((byte) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_byte_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_BYTE_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_BYTE_SARL, AS_BYTE_JAVA);
	}

	private static final String AS_SHORT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : short {",
			"    left as short",
			"  }",
			"}");

	private static final String AS_SHORT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public short fct(final long left) {",
			"    return ((short) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_short_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_SHORT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_SHORT_SARL, AS_SHORT_JAVA);
	}

	private static final String AS_INT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : int {",
			"    left as int",
			"  }",
			"}");

	private static final String AS_INT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public int fct(final long left) {",
			"    return ((int) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_int_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_INT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_int(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_INT_SARL, AS_INT_JAVA);
	}

	private static final String AS_LONG_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : long {",
			"    left as long",
			"  }",
			"}");

	private static final String AS_LONG_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public long fct(final long left) {",
			"    return left;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_long_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_LONG_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_LONG_SARL, AS_LONG_JAVA);
	}

	private static final String AS_FLOAT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : float {",
			"    left as float",
			"  }",
			"}");

	private static final String AS_FLOAT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public float fct(final long left) {",
			"    return left;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_float_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_FLOAT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_FLOAT_SARL, AS_FLOAT_JAVA);
	}

	private static final String AS_DOUBLE_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : double {",
			"    left as double",
			"  }",
			"}");

	private static final String AS_DOUBLE_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public double fct(final long left) {",
			"    return left;",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_double_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_DOUBLE_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_DOUBLE_SARL, AS_DOUBLE_JAVA);
	}

	private static final String AS_BYTE_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Byte {",
			"    left as Byte",
			"  }",
			"}");

	private static final String AS_BYTE_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Byte fct(final long left) {",
			"    return Byte.valueOf((byte) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Byte_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_BYTE_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION,
				"'toByte'");
	}

	@GlobalCompilationTestContribution
	public void  as_Byte(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_BYTE_OBJECT_SARL, AS_BYTE_OBJECT_JAVA);
	}

	private static final String AS_SHORT_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Short {",
			"    left as Short",
			"  }",
			"}");

	private static final String AS_SHORT_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Short fct(final long left) {",
			"    return Short.valueOf((short) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Short_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_SHORT_OBJECT_SARL))
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
	public void  as_Short(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_SHORT_OBJECT_SARL, AS_SHORT_OBJECT_JAVA);
	}

	private static final String AS_INTEGER_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Integer {",
			"    left as Integer",
			"  }",
			"}");

	private static final String AS_INTEGER_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Integer fct(final long left) {",
			"    return Integer.valueOf((int) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Integer_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_INTEGER_OBJECT_SARL))
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
	public void  as_Integer(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_INTEGER_OBJECT_SARL, AS_INTEGER_OBJECT_JAVA);
	}

	private static final String AS_LONG_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Long {",
			"    left as Long",
			"  }",
			"}");

	private static final String AS_LONG_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Long fct(final long left) {",
			"    return Long.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Long_issues() throws Exception {
		SarlScript script = file(getParseHelper(), AS_LONG_OBJECT_SARL);
		validate(getValidationHelper(), getInjector(), script)
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"long to Long")
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_Long(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_LONG_OBJECT_SARL, AS_LONG_OBJECT_JAVA);
	}

	private static final String AS_FLOAT_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Float {",
			"    left as Float",
			"  }",
			"}");

	private static final String AS_FLOAT_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Float fct(final long left) {",
			"    return Float.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Float_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_FLOAT_OBJECT_SARL))
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
	public void  as_Float(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_FLOAT_OBJECT_SARL, AS_FLOAT_OBJECT_JAVA);
	}

	private static final String AS_DOUBLE_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Double {",
			"    left as Double",
			"  }",
			"}");

	private static final String AS_DOUBLE_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Double fct(final long left) {",
			"    return Double.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Double_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_DOUBLE_OBJECT_SARL))
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
	public void  as_Double(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_DOUBLE_OBJECT_SARL, AS_DOUBLE_OBJECT_JAVA);
	}

	private static final String AS_ATOMICINTEGER_OBJECT_SARL = multilineString(
			"import java.util.concurrent.atomic.AtomicInteger",
			"class A {",
			"  def fct(left : long) : AtomicInteger {",
			"    left as AtomicInteger",
			"  }",
			"}");

	private static final String AS_ATOMICINTEGER_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.concurrent.atomic.AtomicInteger;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicInteger fct(final long left) {",
			"    return new AtomicInteger((int) left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_AtomicInteger_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_ATOMICINTEGER_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_AtomicInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_ATOMICINTEGER_OBJECT_SARL, AS_ATOMICINTEGER_OBJECT_JAVA);
	}

	private static final String AS_ATOMICLONG_OBJECT_SARL = multilineString(
			"import java.util.concurrent.atomic.AtomicLong",
			"class A {",
			"  def fct(left : long) : AtomicLong {",
			"    left as AtomicLong",
			"  }",
			"}");

	private static final String AS_ATOMICLONG_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.util.concurrent.atomic.AtomicLong;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicLong fct(final long left) {",
			"    return new AtomicLong(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_AtomicLong_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_ATOMICLONG_OBJECT_SARL))
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
	public void  as_AtomicLong(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_ATOMICLONG_OBJECT_SARL, AS_ATOMICLONG_OBJECT_JAVA);
	}

	private static final String AS_ATOMICDOUBLE_OBJECT_SARL = multilineString(
			"import com.google.common.util.concurrent.AtomicDouble",
			"class A {",
			"  def fct(left : long) : AtomicDouble {",
			"    left as AtomicDouble",
			"  }",
			"}");

	private static final String AS_ATOMICDOUBLE_OBJECT_JAVA = multilineString(
			"import com.google.common.util.concurrent.AtomicDouble;",
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public AtomicDouble fct(final long left) {",
			"    return new AtomicDouble(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_AtomicDouble_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_ATOMICDOUBLE_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_AtomicDouble(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_ATOMICDOUBLE_OBJECT_SARL, AS_ATOMICDOUBLE_OBJECT_JAVA);
	}

	private static final String AS_BIGINTEGER_OBJECT_SARL = multilineString(
			"import java.math.BigInteger",
			"class A {",
			"  def fct(left : long) : BigInteger {",
			"    left as BigInteger",
			"  }",
			"}");

	private static final String AS_BIGINTEGER_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.math.BigInteger;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public BigInteger fct(final long left) {",
			"    return BigInteger.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_BigInteger_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_BIGINTEGER_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_BigInteger(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_BIGINTEGER_OBJECT_SARL, AS_BIGINTEGER_OBJECT_JAVA);
	}

	private static final String AS_BIGDECIMAL_OBJECT_SARL = multilineString(
			"import java.math.BigDecimal",
			"class A {",
			"  def fct(left : long) : BigDecimal {",
			"    left as BigDecimal",
			"  }",
			"}");

	private static final String AS_BIGDECIMAL_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import java.math.BigDecimal;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public BigDecimal fct(final long left) {",
			"    return BigDecimal.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_BigDecimal_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_BIGDECIMAL_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_BigDecimal(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_BIGDECIMAL_OBJECT_SARL, AS_BIGDECIMAL_OBJECT_JAVA);
	}

	private static final String AS_NUMBER_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : Number {",
			"    left as Number",
			"  }",
			"}");

	private static final String AS_NUMBER_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public Number fct(final long left) {",
			"    return Long.valueOf(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_Number_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_NUMBER_OBJECT_SARL))
		.assertNoErrors(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.INVALID_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST,
				"long to Number")
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

	@GlobalCompilationTestContribution
	public void  as_Number(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_NUMBER_OBJECT_SARL, AS_NUMBER_OBJECT_JAVA);
	}

	private static final String AS_STRING_OBJECT_SARL = multilineString(
			"class A {",
			"  def fct(left : long) : String {",
			"    left as String",
			"  }",
			"}");

	private static final String AS_STRING_OBJECT_JAVA = multilineString(
			"import io.sarl.lang.core.annotation.SarlElementType;",
			"import io.sarl.lang.core.annotation.SarlSpecification;",
			"import io.sarl.lang.core.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"import org.eclipse.xtext.xbase.lib.XbaseGenerated;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@XbaseGenerated",
			"@SuppressWarnings(\"all\")",
			"public class A {",
			"  @Pure",
			"  public String fct(final long left) {",
			"    return Long.toString(left);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public A() {",
			"    super();",
			"  }",
			"}",
			"");

	@Test
	public void as_String_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_STRING_OBJECT_SARL))
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
	public void  as_String(ResourceSetGlobalCompilationContext ctx) throws Exception {
		ctx.compileTo(AS_STRING_OBJECT_SARL, AS_STRING_OBJECT_JAVA);
	}

	private static final String AS_UUID_OBJECT_SARL = multilineString(
			"import java.util.UUID",
			"class A {",
			"  def fct(left : long) : UUID {",
			"    left as UUID",
			"  }",
			"}");

	@Test
	public void as_UUID_issues() throws Exception {
		validate(getValidationHelper(), getInjector(), file(getParseHelper(), AS_UUID_OBJECT_SARL))
		.assertNoErrors()
		.assertNoWarnings(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				org.eclipse.xtext.xbase.validation.IssueCodes.OBSOLETE_CAST)
		.assertWarning(
				TypesPackage.eINSTANCE.getJvmParameterizedTypeReference(),
				IssueCodes.POTENTIAL_INEFFICIENT_VALUE_CONVERSION);
	}

}
