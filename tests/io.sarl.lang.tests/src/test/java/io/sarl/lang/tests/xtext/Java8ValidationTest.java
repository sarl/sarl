/*******************************************************************************
 * Copyright (c) 2015 itemis AG (http://www.itemis.eu) and others.
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution, and is available at
 * http://www.eclipse.org/legal/epl-v10.html
 *******************************************************************************/
package io.sarl.lang.tests.xtext;

import static io.sarl.tests.api.tools.TestEObjects.file;
import static io.sarl.tests.api.tools.TestUtils.multilineString;
import static io.sarl.tests.api.tools.TestValidator.validate;

import org.eclipse.xtend.core.validation.IssueCodes;
import org.eclipse.xtext.xbase.XbasePackage;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Tag;
import org.junit.jupiter.api.Test;

import io.sarl.lang.sarl.SarlPackage;
import io.sarl.lang.sarl.SarlScript;
import io.sarl.tests.api.AbstractSarlTest;

/**
 * @author $Author: sgalland$
 * @version $Name$ $Revision$ $Date$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 * @see "https://github.com/eclipse/xtext-xtend/pull/192"
 */
@SuppressWarnings("all")
@DisplayName("SARL compliance to the Java 8 OO standards")
@Tag("core")
@Tag("sarlValidation")
public class Java8ValidationTest extends AbstractSarlTest {

	/**
	private static class InheritedDefaultMethod {
		interface A {
			default void foo() { }
		}
		class C implements A {
			public void bar() { foo(); }
		}
	}
	 */
	@Test
	public void testInheritedDefaultMethod() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"class C implements A {",
				"	def bar { foo }",
				"}"));
		validate(this.getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class RedeclaredMethodFromObject {
		interface A {
			String toString();
		}
		class C implements A {
		}
	}
	 */
	@Test
	public void testRedeclaredMethodFromObject() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	override toString : String",
				"}",
				"class C implements A {}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class RedeclaredMethodFromCustomClass {
		interface A {
			void m();
		}
		class B {
			public void m() { }
		}
		class C extends B { }
		class D extends C implements A { }
	}
	 */
	@Test
	public void testRedeclaredMethodFromCustomClass() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def m",
				"}",
				"class B {",
				"	def m {}",
				"}",
				"class C extends B {}",
				"class D extends C implements A {}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	 private static class ConflictingDefaultMethods01 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		class C implements A, B { }
	}
	*/

	@Test
	public void testConflictingDefaultMethods01() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"class C implements A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type C inherits multiple implementations of the method foo() from A and B.");
	}

	/**
	private static class ConflictingDefaultMethods02 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C extends A, B { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods02() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C extends A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlInterface(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type C inherits multiple implementations of the method foo() from A and B.");
	}

	/**
	private static class ConflictingDefaultMethods03 {
		interface A {
			default void foo() { }
		}
		interface B {
			void foo();
		}
		class C implements A, B { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods03() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo",
				"}",
				"class C implements A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The non-abstract method foo() inherited from A conflicts with the method foo() inherited from B.");
	}

	/**
	private static class ConflictingDefaultMethods04 {
		interface A {
			default void foo() { }
		}
		interface B {
			void foo();
		}
		interface C extends A, B { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods04() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo",
				"}",
				"interface C extends A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlInterface(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The non-abstract method foo() inherited from A conflicts with the method foo() inherited from B.");
	}

	/**
	private static class ConflictingDefaultMethods05 {
		interface A {
			default int foo(java.util.List<String> list) { return 0; }
		}
		interface B {
			default double foo(java.util.List<Class<?>> list) { return 0; }
		}
		class C implements A, B { }
		private void t(C c) {
			java.util.List<String> list1 = null;
			java.util.List<Class<?>> list2 = null;
			c.foo(list1);
			c.foo(list2);
		}
	}
	 */
	@Test
	public void testConflictingDefaultMethods05() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo(list : java.util.List<String>) : int { 0 }",
				"}",
				"interface B {",
				"	def foo(list : java.util.List<Class<?>>) : double { 0 }",
				"}",
				"class C implements A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type C inherits multiple implementations of the method foo(List<String>) from A and B.");
	}
	
	/**
	private static class ConflictingDefaultMethods06 {
		interface A {
			default int foo(java.util.List<String> list) { return 0; }
		}
		interface B {
			default double foo(java.util.List<Class<?>> list) { return 0; }
		}
		interface C extends A, B { }
		private void t(C c) {
			java.util.List<String> list1 = null;
			java.util.List<Class<?>> list2 = null;
			c.foo(list1);
			c.foo(list2);
		}
	}
	 */
	@Test
	public void testConflictingDefaultMethods06() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo(list : java.util.List<String>) : int { 0 }",
				"}",
				"interface B {",
				"	def foo(list : java.util.List<Class<?>>) : double { 0 }",
				"}",
				"interface C extends A, B { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlInterface(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type C inherits multiple implementations of the method foo(List<String>) from A and B.");
	}

	/**
	private static class ConflictingDefaultMethods07 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C {
			default void foo() { }
		}
		interface D extends A, B {
			default void foo() { }
		}
		class E implements A, B, C, D { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods07() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C {",
				"	def foo { }",
				"}",
				"interface D extends A, B {",
				"	override foo { }",
				"}",
				"class E implements A, B, C, D { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type E inherits multiple implementations of the method foo() from C and D.");
	}

	/**
	private static class ConflictingDefaultMethods08 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C {
			default void foo() { }
		}
		interface D extends A, B {
			default void foo() { }
		}
		class E implements A, B, D, C { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods08() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C {",
				"	def foo { }",
				"}",
				"interface D extends A, B {",
				"	override foo { }",
				"}",
				"class E implements A, B, D, C { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The type E inherits multiple implementations of the method foo() from D and C.");
	}

	/**
	private static class ConflictingDefaultMethods09 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C {
			void foo();
		}
		interface D extends A, B {
			default void foo() { }
		}
		class E implements A, B, D, C { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods09() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo",
				"}",
				"interface C {",
				"	def foo",
				"}",
				"interface D extends A, B {",
				"	override foo { }",
				"}",
				"class E implements A, B, D, C { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The non-abstract method foo() inherited from D conflicts with the method foo() inherited from C.");
	}

	/**
	private static class ConflictingDefaultMethods10 {
		interface A {
			default void foo() { }
		}
		interface B {
			void foo()
		}
		interface C {
			void foo();
		}
		interface D extends A, B {
			default void foo() { }
		}
		class E implements B, C, A, D { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods10() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo",
				"}",
				"interface C {",
				"	def foo",
				"}",
				"interface D extends A, B {",
				"	override foo { }",
				"}",
				"class E implements B, C, A, D { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The non-abstract method foo() inherited from D conflicts with the method foo() inherited from C.");
	}

	/**
	private static class ConflictingDefaultMethods11 {
		interface I {
			default void m() { }
		}
		interface J {
			void m();
		}
		interface J1 extends J {
		}
		class E implements I, J1 { }
	}
	 */
	@Test
	public void testConflictingDefaultMethods11() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface I {",
				"	def m {}",
				"}",
				"interface J {",
				"	def m",
				"}",
				"interface J1 extends J {}",
				"class E implements I, J1 {}"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"The non-abstract method m() inherited from I conflicts with the method m() inherited from J.");
	}

	/**
	private static class MissingImplementation_01 {
		abstract class A {
			public abstract void m();
		}
		abstract class B extends A {
		}
		interface I {
			default void m() { }
		}
		class C extends B implements I { }
	}
	 */
	@Test
	public void testMissingImplementation_01() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"abstract class A {",
				"	def m",
				"}",
				"abstract class B extends A {}",
				"interface I {",
				"	def m {}",
				"}",
				"class C extends B implements I {}"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CLASS_MUST_BE_ABSTRACT,
				"The class C must be defined abstract because it does not implement m()");
	}

	/**
	private static class MissingImplementation_02 {
		interface I {
			default void m() { }
		}
		interface J extends I {
			void m();
		}
		class C implements J { }
	}
	 */
	@Test
	public void testMissingImplementation_02() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface I {",
				"	def m {}",
				"}",
				"interface J extends I {",
				"	override m",
				"}",
				"class C implements J {}"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CLASS_MUST_BE_ABSTRACT,
				"The class C must be defined abstract because it does not implement m()");
	}

	/**
	private static class MissingImplementation_02 {
		interface I {
			default void m() { }
		}
		interface J extends I {
		}
		interface K extends J {
			void m();
		}
		interface L extends K { }
		class C implements I, K, L { }
	}
	 */
	@Test
	public void testMissingImplementation_03() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface I {",
				"	def m {}",
				"}",
				"interface J extends I {",
				"}",
				"interface K extends J {",
				"	override m",
				"}",
				"interface L extends J {}",
				"class C implements I, K, L {}"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CLASS_MUST_BE_ABSTRACT,
				"The class C must be defined abstract because it does not implement m()");
	}

	/**
	private static class ResolvedConflict01 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		class C implements A, B {
			public void foo() { }
		}
	}
	 */
	@Test
	public void testResolvedConflict01() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"class C implements A, B {",
				"	override foo { }",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedConflict02 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C extends A, B {
			default void foo() { }
		}
		class D implements A, B, C { }
	}
	 */
	@Test
	public void testResolvedConflict02() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C extends A, B {",
				"	override foo { }",
				"}",
				"class D implements /*A, B,*/ C { }"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedConflict03 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C extends A, B {
			default void foo() { }
		}
		class D implements A, C, B { }
	}
	 */
	@Test
	public void testResolvedConflict03() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C extends A, B {",
				"	override foo { }",
				"}",
				"class D implements /*A,*/ C/*, B*/ { }"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedConflict04 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C extends A, B {
			default void foo() { }
		}
		class D implements C, A, B { }
	}
	 */
	@Test
	public void testResolvedConflict04() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C extends A, B {",
				"	override foo { }",
				"}",
				"class D implements C/*, A, B*/ { }"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedConflict05 {
		interface I {
			default void m() { }
		}
		interface J extends I {
		}
		interface K extends J {
			default void m() { }
		}
		interface L extends J {
		}
		class C implements I, J, L { }
	}
	 */
	@Test
	public void testResolvedConflict05() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface I {",
				"	def m {}",
				"}",
				"interface J extends I {",
				"}",
				"interface K extends J {",
				"	override m {}",
				"}",
				"interface L extends J {}",
				"class C implements /*I, J,*/ L {}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class InheritedConflict01 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		class C implements A, B {
		}
		class D extends C { }
	}
	 */
	@Test
	public void testInheritedConflict01() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"class C implements A, B { }",
				"class D extends C { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlClass(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"");
	}

	/**
	private static class InheritedConflict02 {
		interface A {
			default void foo() { }
		}
		interface B {
			default void foo() { }
		}
		interface C extends A, B {
		}
		interface D extends C { }
	}
	 */
	@Test
	public void testInheritedConflict02() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo { }",
				"}",
				"interface B {",
				"	def foo { }",
				"}",
				"interface C extends A, B { }",
				"interface D extends C { }"));
		validate(getValidationHelper(), getInjector(), file).assertError(SarlPackage.eINSTANCE.getSarlInterface(), IssueCodes.CONFLICTING_DEFAULT_METHODS,
				"");
	}

	/**
	private static class AbstractMethodCall {
		interface A {
			void foo();
		}
		class E implements A {
			public void foo() {
				A.super.foo();
			}
		}
	}
	 */
	@Test
	public void testAbstractMethodCall() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"interface A {",
				"	def foo",
				"}",
				"class E implements A {",
				"	override foo {",
				"		A.super.foo",
				"	}",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertError(XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				org.eclipse.xtext.xbase.validation.IssueCodes.ABSTRACT_METHOD_INVOCATION,
				"Cannot directly invoke the abstract method foo() of the type A");
	}

	/**
	private static class InterfaceSuperCall {
		class Foo {
			public void foo() {
				java.util.List.super.clear();
			}
		}
	}
	 */
	@Test
	public void testInterfaceSuperCall() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"class Foo {",
				"	def foo {",
				"		java.util.List.super.clear",
				"	}",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertError(XbasePackage.eINSTANCE.getXMemberFeatureCall(),
				org.eclipse.xtext.xbase.validation.IssueCodes.NO_ENCLOSING_INSTANCE_AVAILABLE,
				"The enclosing type does not extend or implement the interface List");
	}

	/**
	private static class ResolvedByAbstractType01 {
		class A<T> extends java.util.AbstractCollection<T> implements java.util.Set<T> {
			public java.util.Iterator<T> iterator() {
				return null;
			}
			public int size() {
				return 0;
			}
		}
		class Test {
			public void test(java.util.AbstractCollection<?> a) {
				a.spliterator();
			}
			public void test(A<?> a) {
				a.spliterator();
			}
		}
	}
	*/
	@Test
	public void testResolvedByAbstractType01() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"import java.util.Iterator",
				"import java.util.Set",
				"import java.util.AbstractCollection",
				"class A<T> extends AbstractCollection<T> implements Set<T> {",
				"  override iterator : Iterator<T> {",
				"    null",
				"  }",
				"  override size : int {",
				"    0",
				"  }",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedByAbstractType02 {
		abstract class B<T> extends java.util.AbstractCollection<T> {
			public java.util.Spliterator<T> splititerator() {
				return null;
			}
		}
		class A<T> extends B<T> implements java.util.Set<T> {
			public java.util.Iterator<T> iterator() {
				return null;
			}
			public int size() {
				return 0;
			}
		}
		class Test {
			public void test(java.util.AbstractCollection<?> a) {
				a.spliterator();
			}
			public void test(A<?> a) {
				a.spliterator();
			}
		}
	}
	*/
	@Test
	public void testResolvedByAbstractType02() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"import java.util.Iterator",
				"import java.util.Set",
				"import java.util.AbstractCollection",
				"abstract class B<T> extends AbstractCollection<T> {",
				"  def splititrator : java.util.Spliterator<T> {",
				"    null",
				"  }",
				"}",
				"class A<T> extends B<T> implements Set<T> {",
				"  override iterator : Iterator<T> {",
				"    null",
				"  }",
				"  override size : int {",
				"    0",
				"  }",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}

	/**
	private static class ResolvedByAbstractType03 {
		interface I<T> extends java.util.Set<T> {
			
		}
		class A<T> extends java.util.AbstractCollection<T> implements I<T> {
			public java.util.Iterator<T> iterator() {
				return null;
			}
			public int size() {
				return 0;
			}
		}
		class Test {
			public void test(java.util.AbstractCollection<?> a) {
				a.spliterator();
			}
			public void test(A<?> a) {
				a.spliterator();
			}
		}
	}
	*/
	@Test
	public void testResolvedByAbstractType03() throws Exception {
		SarlScript file = file(getParseHelper(), multilineString(
				"import java.util.Iterator",
				"import java.util.Set",
				"import java.util.AbstractCollection",
				"interface I<T> extends Set<T> {",
				"}",
				"class A<T> extends AbstractCollection<T> implements I<T> {",
				"  override iterator : Iterator<T> {",
				"    null",
				"  }",
				"  override size : int {",
				"    0",
				"  }",
				"}"));
		validate(getValidationHelper(), getInjector(), file).assertNoErrors();
	}


}
