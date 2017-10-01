/*
 * Copyright (C) 2014-2017 the original authors or authors.
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

package io.sarl.lang.tests.bugs.to00999;

import static org.junit.Assert.*;

import com.google.common.base.Objects;
import com.google.inject.Inject;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.junit.Test;

import io.sarl.lang.SARLVersion;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;
import io.sarl.tests.api.AbstractSarlTest.Validator;

/** Testing class for issue: Invalid "int == Integer".
 *
 * <p>https://github.com/sarl/sarl/issues/764
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@SuppressWarnings("all")
public class Bug764 extends AbstractSarlTest {

	private static final String SNIPSET1 = multilineString(
			"package io.sarl.lang.tests.bug764",
			"class X {",
			"	def fct (a : int, b : int) : boolean {",
			"      a == b",
			"   }",
			"	def fct (a : Integer, b : Integer) : boolean {",
			"      a == b",
			"   }",
			"	def fct (a : int, b : Integer) : boolean {",
			"      a == b",
			"   }",
			"	def fct (a : Integer, b : int) : boolean {",
			"      a == b",
			"   }",
			"}");

	private final String EXPECTED1 = multilineString(
			"package io.sarl.lang.tests.bug764;",
			"",
			"import com.google.common.base.Objects;",
			"import io.sarl.lang.annotation.SarlElementType;",
			"import io.sarl.lang.annotation.SarlSpecification;",
			"import io.sarl.lang.annotation.SyntheticMember;",
			"import org.eclipse.xtext.xbase.lib.Pure;",
			"",
			"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
			"@SarlElementType(" + SarlPackage.SARL_CLASS + ")",
			"@SuppressWarnings(\"all\")",
			"public class X {",
			"  @Pure",
			"  public boolean fct(final int a, final int b) {",
			"    return (a == b);",
			"  }",
			"  ",
			"  @Pure",
			"  public boolean fct(final Integer a, final Integer b) {",
			"    return Objects.equal(a, b);",
			"  }",
			"  ",
			"  @Pure",
			"  public boolean fct(final int a, final Integer b) {",
			"    return (a == (b).intValue());",
			"  }",
			"  ",
			"  @Pure",
			"  public boolean fct(final Integer a, final int b) {",
			"    return ((a).intValue() == b);",
			"  }",
			"  ",
			"  @SyntheticMember",
			"  public X() {",
			"    super();",
			"  }",
			"}",
			"");

	@Inject
	private CompilationTestHelper compiler;

	@Test
	public void parsing_01() throws Exception {
		SarlScript mas = file(SNIPSET1);
		final Validator validator = validate(mas);
		validator.assertNoErrors();
	}

	@Test
	public void compiling_01() throws Exception {
		this.compiler.compile(SNIPSET1, (it) -> {
			final String actual = it.getGeneratedCode("io.sarl.lang.tests.bug764.X");
			assertEquals(EXPECTED1, actual);
		});
	}

	@Test
	public void testEquality_int_int() {
		int a = 5;
		int b = 5;
		assertTrue(a == b);
	}

	@Test
	public void testEquality_int_Integer() {
		int a = 5;
		Integer b = Integer.valueOf(5);
		assertTrue(a == b.intValue());
	}

	@Test
	public void testEquality_Integer_int() {
		Integer a = Integer.valueOf(5);
		int b = 5;
		assertTrue(a.intValue() == b);
	}

	@Test
	public void testEquality_Integer_Integr() {
		Integer a = Integer.valueOf(5);
		Integer b = Integer.valueOf(5);
		assertTrue(Objects.equal(a, b));
	}

}

