/*
 * $Id$
 *
 * SARL is an general-purpose behavior programming language.
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

/** Test the outline of the "behavior" statement.
 *
 * @author $Author: sgalland$
 * @version $FullVersion$
 * @mavengroupid $GroupId$
 * @mavenartifactid $ArtifactId$
 */
public class BehaviorOutlineTest extends AbstractSARLOutlineTreeProviderTest {

	/**
	 * @throws Exception
	 */
	@Test
	public void empty() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 {}"); //$NON-NLS-1$
		asserts.numChildren(1);
		asserts.leaf(0, "B1"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1Attribute() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 { var attr : boolean }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2Attributes() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 { val xyz : float var attr : boolean }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
		a.leaf(1, "xyz : float"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1BehaviorUnit_0() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 { on io.sarl.lang.core.Event {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "on Event"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test1BehaviorUnit_1() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 { on io.sarl.lang.core.Event [true] {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(1);
		a.leaf(0, "on Event [true]"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void test2BehaviorUnits() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 { on io.sarl.lang.core.Event [true] {} on io.sarl.lang.core.Event {} }"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(2);
		a.leaf(0, "on Event"); //$NON-NLS-1$
		a.leaf(1, "on Event [true]"); //$NON-NLS-1$
	}

	/**
	 * @throws Exception
	 */
	@Test
	public void testMany() throws Exception {
		OutlineAsserts asserts = newOutlineAsserts(
				"behavior B1 {\n" //$NON-NLS-1$
				+ "on io.sarl.lang.core.Event [true] {}\n" //$NON-NLS-1$
				+ "val attr : boolean\n" //$NON-NLS-1$
				+ "def fct1(a : char) {}\n" //$NON-NLS-1$
				+ "val xyz = 5\n" //$NON-NLS-1$
				+ "def fct2 : int {1}\n" //$NON-NLS-1$
				+ "}"); //$NON-NLS-1$
		asserts.numChildren(1);
		OutlineAsserts a;
		a = asserts.child(0, "B1"); //$NON-NLS-1$
		a.numChildren(5);
		a.leaf(0, "attr : boolean"); //$NON-NLS-1$
		a.leaf(1, "xyz : int"); //$NON-NLS-1$
		a.leaf(2, "on Event [true]"); //$NON-NLS-1$
		a.leaf(3, "fct1(char) : void"); //$NON-NLS-1$
		a.leaf(4, "fct2() : int"); //$NON-NLS-1$
	}

}
