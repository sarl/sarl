/*
 * $Id$
 *
 * SARL is an general-purpose skill programming language.
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
package io.sarl.lang.ui.tests.outline;

import org.junit.Test;

/** Test the outline of the "skill" statement.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class SkillOutlineTest extends AbstractSARLOutlineTreeProviderTest {

	/**
	 * @throws Exception
	 */
	@Test
	public void empty() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity {}"); //$NON-NLS-1$
		asserts.numChildren(1);
		asserts.leaf(0, "S1"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Attribute() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { var attr : boolean }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Attributes() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { val xyz : float var attr : boolean }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
		a.leaf(1, "xyz : float"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { def operation : boolean {true} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "operation() : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { def operation(a : float) {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "operation(float) : void"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Action_2() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { def operation(a : float) : int {1} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "operation(float) : int"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Actions_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { def xyz : boolean {true} def abc : float {1.0} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "abc() : float"); //$NON-NLS-1$
		a.leaf(1, "xyz() : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Actions_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { def xyz(b : char) : boolean {true} def abc : float {1.0} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "abc() : float"); //$NON-NLS-1$
		a.leaf(1, "xyz(char) : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Constructor_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { new(a : boolean) {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "new(boolean)"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Constructor_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { new() {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "new()"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Constructors() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity { new(a : float) {} new(z : char) {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "new(char)"); //$NON-NLS-1$
		a.leaf(1, "new(float)"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void testMany() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"skill S1 extends io.sarl.lang.core.Capacity {\n" //$NON-NLS-1$
				+ "val attr : boolean\n" //$NON-NLS-1$
				+ "def fct1(a : char) {}\n" //$NON-NLS-1$
				+ "val xyz = 5\n" //$NON-NLS-1$
				+ "def fct2 : int {1}\n" //$NON-NLS-1$
				+ "}"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "S1"); //$NON-NLS-1$
		a.numChildren(4);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
		a.leaf(1, "xyz : int"); //$NON-NLS-1$
		a.leaf(2, "fct1(char) : void"); //$NON-NLS-1$
		a.leaf(3, "fct2() : int"); //$NON-NLS-1$
	}

}
