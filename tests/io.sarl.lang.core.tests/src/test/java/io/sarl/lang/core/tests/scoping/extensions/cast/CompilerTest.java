/*
 * $Id$
 *
 * SARL is an general-purpose agent programming language.
 * More details on http://www.sarl.io
 *
 * Copyright (C) 2014-2019 the original authors or authors.
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

import org.eclipse.xtext.common.types.TypesPackage;
import org.junit.Test;
import org.junit.runner.RunWith;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.validation.IssueCodes;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.MassiveCompilationSuite;
import io.sarl.tests.api.MassiveCompilationSuite.CompilationTest;
import io.sarl.tests.api.MassiveCompilationSuite.Context;

/**
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(MassiveCompilationSuite.class)
@SuppressWarnings("all")
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
	public void string_as_boolean_issues() throws Exception {
		validate(file(STRING_AS_BOOLEAN_SARL))
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

	@CompilationTest
	public static void string_as_boolean(Context ctx) throws Exception {
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
	public void boolean_as_string_issues() throws Exception {
		validate(file(BOOLEAN_AS_STRING_SARL))
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

	@CompilationTest
	public static void boolean_as_string(Context ctx) throws Exception {
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
	public void char_as_string_issues() throws Exception {
		validate(file(CHAR_AS_STRING_SARL))
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

	@CompilationTest
	public static void char_as_string(Context ctx) throws Exception {
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
	public void string_as_byte_issues() throws Exception {
		validate(file(STRING_AS_BYTE_SARL))
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

	@CompilationTest
	public static void string_as_byte(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_BYTE_SARL, STRING_AS_BYTE_JAVA);
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
	public void string_as_short_issues() throws Exception {
		validate(file(STRING_AS_SHORT_SARL))
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

	@CompilationTest
	public static void string_as_short(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_SHORT_SARL, STRING_AS_SHORT_JAVA);
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
	public void string_as_int_issues() throws Exception {
		validate(file(STRING_AS_INT_SARL))
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

	@CompilationTest
	public static void string_as_int(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_INT_SARL, STRING_AS_INT_JAVA);
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
	public void string_as_long_issues() throws Exception {
		validate(file(STRING_AS_LONG_SARL))
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

	@CompilationTest
	public static void string_as_long(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_LONG_SARL, STRING_AS_LONG_JAVA);
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
	public void string_as_float_issues() throws Exception {
		validate(file(STRING_AS_FLOAT_SARL))
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

	@CompilationTest
	public static void string_as_float(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_FLOAT_SARL, STRING_AS_FLOAT_JAVA);
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
	public void string_as_double_issues() throws Exception {
		validate(file(STRING_AS_DOUBLE_SARL))
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

	@CompilationTest
	public static void string_as_double(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_DOUBLE_SARL, STRING_AS_DOUBLE_JAVA);
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
	public void string_as_atomicinteger_issues() throws Exception {
		validate(file(STRING_AS_ATOMICINTEGER_SARL))
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

	@CompilationTest
	public static void string_as_atomicinteger(Context ctx) throws Exception {
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
	public void string_as_atomiclong_issues() throws Exception {
		validate(file(STRING_AS_ATOMICLONG_SARL))
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

	@CompilationTest
	public static void string_as_atomiclong(Context ctx) throws Exception {
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
	public void string_as_atomicdouble_issues() throws Exception {
		validate(file(STRING_AS_ATOMICDOUBLE_SARL))
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

	@CompilationTest
	public static void string_as_atomicdouble(Context ctx) throws Exception {
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
	public void string_as_biginteger_issues() throws Exception {
		validate(file(STRING_AS_BIGINTEGER_SARL))
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

	@CompilationTest
	public static void string_as_biginteger(Context ctx) throws Exception {
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
	public void string_as_bigdecimal_issues() throws Exception {
		validate(file(STRING_AS_BIGDECIMAL_SARL))
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

	@CompilationTest
	public static void string_as_bigdecimal(Context ctx) throws Exception {
		ctx.compileTo(STRING_AS_BIGDECIMAL_SARL, STRING_AS_BIGDECIMAL_JAVA);
	}

}
