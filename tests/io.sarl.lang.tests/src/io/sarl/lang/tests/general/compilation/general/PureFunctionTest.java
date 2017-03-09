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
package io.sarl.lang.tests.general.compilation.general;

import com.google.inject.Inject;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper;
import org.eclipse.xtext.xbase.testing.CompilationTestHelper.Result;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.SARLVersion;
import io.sarl.tests.api.AbstractSarlTest;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	PureFunctionTest.DefinitionTests.class,
})
@SuppressWarnings("all")
public class PureFunctionTest {

	public static class DefinitionTests extends AbstractSarlTest {

		@Inject
		private CompilationTestHelper compiler;

		@Test
		public void noPureParent_noPureLocal() throws Exception {
			String source = multilineString(
					"class C1 {",
					"  def fct { return Math::random }",
					"}",
					"class C2 extends C1 {",
					"  override fct {",
					"    return Math::random",
					"  }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> {
					assertEquals(expectedC1, r.getGeneratedCode("C1"));
					assertEquals(expectedC2, r.getGeneratedCode("C2"));
				});
		}

		@Test
		public void noPureParent_pureLocal_tooComplexToBeDetected() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def fct { return Math.random }",
					"}",
					"class C2 extends C1 {",
					"	override fct : double { 0 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  @Override",
					"  public double fct() {",
					"    return 0;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1, r.getGeneratedCode("C1"));
					assertEquals(expectedC2, r.getGeneratedCode("C2"));
				}
			});
		}

		@Test
		public void pureParent_pureLocal_tooComplexToBeDetected() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def fct { }",
					"}",
					"class C2 extends C1 {",
					"	override fct { }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public void fct() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  public void fct() {",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1, r.getGeneratedCode("C1"));
					assertEquals(expectedC2, r.getGeneratedCode("C2"));
				}
			});
		}

		@Test
		public void abstractParent_noPureLocal() throws Exception {
			String source = multilineString(
					"abstract class C1 {",
					"  abstract def fct : double",
					"}",
					"class C2 extends C1 {",
					"  override fct {",
					"    return Math::random",
					"  }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class C1 {",
					"  public abstract double fct();",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, (r) -> {
					assertEquals(expectedC1, r.getGeneratedCode("C1"));
					assertEquals(expectedC2, r.getGeneratedCode("C2"));
				});
		}

		@Test
		public void abstractParent_pureLocal_tooComplexToBeDetected() throws Exception {
			String source = multilineString(
					"abstract class C1 {",
					"	abstract def fct : double",
					"}",
					"class C2 extends C1 {",
					"	override fct : double { 0 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public abstract class C1 {",
					"  public abstract double fct();",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Inline(value = \"0.0\", constantExpression = true)",
					"  @Override",
					"  public double fct() {",
					"    return 0;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C2() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.compile(source, new IAcceptor<CompilationTestHelper.Result>() {
				@Override
				public void accept(Result r) {
					assertEquals(expectedC1, r.getGeneratedCode("C1"));
					assertEquals(expectedC2, r.getGeneratedCode("C2"));
				}
			});
		}

		@Test
		public void special_get() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def getXXX : double { 9 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"9.0\", constantExpression = true)",
					"  @Pure",
					"  public double getXXX() {",
					"    return 9;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

		@Test
		public void special_is() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def isXXX : boolean { false }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"false\", constantExpression = true)",
					"  @Pure",
					"  public boolean isXXX() {",
					"    return false;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

		@Test
		public void special_has() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def hasXXX : boolean { false }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"false\", constantExpression = true)",
					"  @Pure",
					"  public boolean hasXXX() {",
					"    return false;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

		@Test
		public void special_toString() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def toString : String { \"\" }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"\\\"\\\"\", constantExpression = true)",
					"  @Pure",
					"  public String toString() {",
					"    return \"\";",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

		@Test
		public void special_hashCode() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def hashCode : int { 0 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"0\", constantExpression = true)",
					"  @Pure",
					"  public int hashCode() {",
					"    return 0;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

		@Test
		public void special_equals() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def equals(a : Object) : boolean { false }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import io.sarl.lang.annotation.SarlSpecification;",
					"import io.sarl.lang.annotation.SyntheticMember;",
					"import org.eclipse.xtext.xbase.lib.Inline;",
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SarlSpecification(\"" + SARLVersion.SPECIFICATION_RELEASE_VERSION_STRING + "\")",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Inline(value = \"false\", constantExpression = true)",
					"  @Pure",
					"  public boolean equals(final Object a) {",
					"    return false;",
					"  }",
					"  ",
					"  @SyntheticMember",
					"  public C1() {",
					"    super();",
					"  }",
					"}",
					""
					);
			this.compiler.assertCompilesTo(source, expectedC1);
		}

	}

}