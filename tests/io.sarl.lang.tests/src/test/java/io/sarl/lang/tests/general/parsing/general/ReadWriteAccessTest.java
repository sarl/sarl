/*
 * Copyright (C) 2014-2018 the original authors or authors.
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
package io.sarl.lang.tests.general.parsing.general;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.junit.runners.Suite;
import org.junit.runners.Suite.SuiteClasses;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/** Test the read/write accesses.
 *
 * FIXME: See issue #809. Remove when Xtext PR is merged: https://github.com/eclipse/xtext-extras/pull/232.
 *
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @since 0.7
 * @see "https://github.com/eclipse/xtext-extras/pull/232"
 */
@RunWith(Suite.class)
@SuiteClasses({
	ReadWriteAccessTest.FieldReadTest.class,
	ReadWriteAccessTest.VariableReadTest.class,
})
@SuppressWarnings("all")
public class ReadWriteAccessTest {
	
	public static class FieldReadTest extends AbstractSarlTest {
	
		@Test
		public void test000() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  val f = 1",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C1.f");
		}
	
		@Test
		public void test001() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C1.f");
		}

		@Test
		public void test002() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    f = 2",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C1.f");
		}

		@Test
		public void test003() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : int {",
					"    var x = f",
					"    return x",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test004() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    f = f + 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test005() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    this.f = 2",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C1.f");
		}

		@Test
		public void test006() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : int {",
					"    var x = this.f",
					"    return x",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test007() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    this.f = this.f + 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test008() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    fct2(this.f)",
					"  }",
					"  def fct2(p : int) : void {",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test009() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var f = 1",
					"  def fct : void {",
					"    for (i : 1..f) { }",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test010() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : C11 {",
					"    return this.x",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C11.f");
		}

		@Test
		public void test011() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : void {",
					"    this.x.f = 4",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					SarlPackage.eINSTANCE.getSarlField(),
					IssueCodes.UNUSED_PRIVATE_MEMBER,
					"field C11.f");
		}

		@Test
		public void test012() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : int {",
					"    return this.x.f",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test013() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : int {",
					"    this.x.f = (this.x.f ** 3) as int",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test014() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct(p : int) : int {",
					"    if (this.x !== null) {",
					"      switch (p) {",
					"      case 1 : { return this.x.f }",
					"      default: { }",
					"      }",
					"    }",
					"    return 0",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test015() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : int {",
					"    return xx().f",
					"  }",
					"  def xx : C11 {",
					"    null",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test016() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct(x : String) : int {",
					"    return (x + 35).f",
					"  }",
					"  def operator_plus(a : String, b : int) : C11 {",
					"    null",
					"  }",
					"  static class C11 {",
					"    var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test017() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected val f = 1",
					"}"
					));
			validate(mas).assertNoIssues();
		}
	
		@Test
		public void test018() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test019() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    f = 2",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test020() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : int {",
					"    var x = f",
					"    return x",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test021() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    f = f + 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test022() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    this.f = 2",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test023() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : int {",
					"    var x = this.f",
					"    return x",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test024() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    this.f = this.f + 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test025() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    fct2(this.f)",
					"  }",
					"  def fct2(p : int) : void {",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test026() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  protected var f = 1",
					"  def fct : void {",
					"    for (i : 1..f) { }",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test027() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : C11 {",
					"    return this.x",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test028() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : void {",
					"    this.x.f = 4",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test029() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : int {",
					"    return this.x.f",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test0130() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct : int {",
					"    this.x.f = (this.x.f ** 3) as int",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test031() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  var x : C11",
					"  def fct(p : int) : int {",
					"    if (this.x !== null) {",
					"      switch (p) {",
					"      case 1 : { return this.x.f }",
					"      default: { }",
					"      }",
					"    }",
					"    return 0",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test032() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : int {",
					"    return xx().f",
					"  }",
					"  def xx : C11 {",
					"    null",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test033() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct(x : String) : int {",
					"    return (x + 35).f",
					"  }",
					"  def operator_plus(a : String, b : int) : C11 {",
					"    null",
					"  }",
					"  static class C11 {",
					"    protected var f = 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

	}

	public static class VariableReadTest extends AbstractSarlTest {
		
		@Test
		public void test000() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : int {",
					"    val f = 1",
					"    1",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXVariableDeclaration(),
					org.eclipse.xtext.xbase.validation.IssueCodes.UNUSED_LOCAL_VARIABLE,
					"variable f");
		}
	
		@Test
		public void test001() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : int {",
					"    var f = 1",
					"    1",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXVariableDeclaration(),
					org.eclipse.xtext.xbase.validation.IssueCodes.UNUSED_LOCAL_VARIABLE,
					"variable f");
		}

		@Test
		public void test002() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : void {",
					"    var f : int",
					"    f = 2",
					"  }",
					"}"
					));
			validate(mas).assertWarning(
					XbasePackage.eINSTANCE.getXVariableDeclaration(),
					org.eclipse.xtext.xbase.validation.IssueCodes.UNUSED_LOCAL_VARIABLE,
					"variable f");
		}

		@Test
		public void test003() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : int {",
					"    var f = 1",
					"    var x = f",
					"    return x",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test004() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : void {",
					"    var f = 1",
					"    f = f + 1",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test005() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : void {",
					"    var f = 1",
					"    fct2(f)",
					"  }",
					"  def fct2(p : int) : void {",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

		@Test
		public void test006() throws Exception {
			SarlScript mas = file(multilineString(
					"class C1 {",
					"  def fct : void {",
					"    var f = 1",
					"    for (i : 1..f) { }",
					"  }",
					"}"
					));
			validate(mas).assertNoIssues();
		}

	}

}
