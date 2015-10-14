/*
 * Copyright (C) 2014-2015 the original authors or authors.
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
package io.sarl.lang.tests.compilation.general;

import static org.junit.Assert.assertEquals;
import org.eclipse.xtext.util.IAcceptor;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper.Result;

import io.sarl.lang.SARLInjectorProvider;
import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.lang.tests.actionprototype.DefaultActionPrototypeProviderTest;
import io.sarl.tests.api.AbstractSarlTest;

import org.eclipse.xtext.XtextPackage;
import org.eclipse.xtext.junit4.InjectWith;
import org.eclipse.xtext.junit4.XtextRunner;
import org.eclipse.xtext.junit4.util.ParseHelper;
import org.eclipse.xtext.junit4.validation.ValidationTestHelper;
import org.eclipse.xtext.xbase.XbasePackage;
import org.eclipse.xtext.xbase.compiler.CompilationTestHelper;
import org.eclipse.xtext.xbase.validation.IssueCodes;
import org.eclipse.xtext.xtype.XtypePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;
import com.google.inject.Inject;


/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
@RunWith(Suite.class)
@SuiteClasses({
	PureFunctionTest.DefinitionTests.class,
	PureFunctionTest.UseTests.class,
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
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  public double fct() {",
					"    return Math.random();",
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
		public void noPureParent_pureLocal() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def fct { return Math.random }",
					"}",
					"class C2 extends C1 {",
					"	override fct : double { 0 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  public double fct() {",
					"    return Math.random();",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  @Pure",
					"  public double fct() {",
					"    return 0;",
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
		public void pureParent_pureLocal() throws Exception {
			String source = multilineString(
					"class C1 {",
					"	def fct { }",
					"}",
					"class C2 extends C1 {",
					"	override fct { }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C1 {",
					"  @Pure",
					"  public void fct() {",
					"  }",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  @Pure",
					"  public void fct() {",
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
					"@SuppressWarnings(\"all\")",
					"public abstract class C1 {",
					"  public abstract double fct();",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  public double fct() {",
					"    return Math.random();",
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
		public void abstractParent_pureLocal() throws Exception {
			String source = multilineString(
					"abstract class C1 {",
					"	abstract def fct : double",
					"}",
					"class C2 extends C1 {",
					"	override fct : double { 0 }",
					"}",
					"");
			final String expectedC1 = multilineString(
					"@SuppressWarnings(\"all\")",
					"public abstract class C1 {",
					"  public abstract double fct();",
					"}",
					""
					);
			final String expectedC2 = multilineString(
					"import org.eclipse.xtext.xbase.lib.Pure;",
					"",
					"@SuppressWarnings(\"all\")",
					"public class C2 extends C1 {",
					"  @Override",
					"  @Pure",
					"  public double fct() {",
					"    return 0;",
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

	}

	public static class UseTests extends AbstractSarlTest {

		@Test
		public void invalidUsageOfPureFunction() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"   var attr = 5",
					"	def purefct : int { 5 }",
					"}",
					"behavior B2 {",
					"   var inst = new B1",
					"	def testfct {",
					"      inst.purefct",
					"   }",
					"}",
					""));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
				36, 2,
				"Invalid supertype. Expecting a class");
		}

		@Test
		public void validUsageOfPureFunction() throws Exception {
			SarlScript mas = file(multilineString(
					"behavior B1 {",
					"   new () { super(null) }",
					"   var attr = 5",
					"	def purefct : int { 5 }",
					"}",
					"behavior B2 {",
					"   new () { super(null) }",
					"   var inst = new B1",
					"	def testfct : int {",
					"      inst.purefct",
					"   }",
					"}",
					""));
			validate(mas).assertError(
				SarlPackage.eINSTANCE.getSarlBehavior(),
				org.eclipse.xtend.core.validation.IssueCodes.CLASS_EXPECTED,
				36, 2,
				"Invalid supertype. Expecting a class");
		}

	}

}